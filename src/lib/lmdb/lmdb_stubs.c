/* Ithaca - Audio fingerprinting
 * Copyright (C) 2026 Romain Beauxis
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */

#include <caml/alloc.h>
#include <caml/callback.h>
#include <caml/custom.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/threads.h>

#include <errno.h>
#include <lmdb.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define ITHACA_DB "db"
#define ITHACA_SATURATED_DB "saturated"
#define ITHACA_PROFILE "profile"
#define ITHACA_PROFILE_KEY "profile"

static void lmdb_env_finalize(value v)
{
  MDB_env *env = *((MDB_env **)Data_custom_val(v));
  if (env)
    mdb_env_close(env);
}

static struct custom_operations lmdb_env_ops = {
    "ithaca.lmdb_env",          lmdb_env_finalize,
    custom_compare_default,     custom_hash_default,
    custom_serialize_default,   custom_deserialize_default,
    custom_compare_ext_default, custom_fixed_length_default};

#define Env_val(v) (*((MDB_env **)Data_custom_val(v)))

CAMLprim value ocaml_lmdb_string_of_error(value code)
{
  CAMLparam1(code);
  CAMLreturn(caml_copy_string(mdb_strerror(Int_val(code))));
}

CAMLprim value ocaml_lmdb_close(value env)
{
  CAMLparam1(env);
  MDB_env *e = Env_val(env);
  if (e) {
    mdb_env_close(e);
    *((MDB_env **)Data_custom_val(env)) = NULL;
  }
  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lmdb_open(value file)
{
  CAMLparam1(file);
  CAMLlocal1(wrapper);
  uint64_t n = 1024;
  size_t map_size = n * n * n * n;
  MDB_env *env = NULL;
  int rc;

  rc = mdb_env_create(&env);
  if (rc != 0)
    caml_raise_with_arg(*caml_named_value("ocaml_lmdb_error"), Val_int(rc));

  mdb_env_set_mapsize(env, map_size);
  mdb_env_set_maxdbs(env, 3);
  mdb_env_set_maxreaders(env, 4096);

  rc = mdb_env_open(env, String_val(file), MDB_NOTLS | MDB_NOSUBDIR, 0755);
  if (rc != 0) {
    mdb_env_close(env);
    caml_raise_with_arg(*caml_named_value("ocaml_lmdb_error"), Val_int(rc));
  }

  wrapper = caml_alloc_custom(&lmdb_env_ops, sizeof(MDB_env *), 0, 1);
  *((MDB_env **)Data_custom_val(wrapper)) = env;

  CAMLreturn(wrapper);
}

#define check_error(cursor, txn, ret)                                          \
  do {                                                                         \
    int _tmp = (ret);                                                          \
    if (_tmp != 0) {                                                           \
      if (_tmp == MDB_NOTFOUND) {                                              \
        if (cursor) {                                                          \
          mdb_cursor_close(cursor);                                            \
        }                                                                      \
        if (txn) {                                                             \
          mdb_txn_commit(txn);                                                 \
        }                                                                      \
        caml_raise_not_found();                                                \
      } else {                                                                 \
        fprintf(stderr, "lmdb_stubs.c error at line %d\n", __LINE__);          \
        if (cursor) {                                                          \
          mdb_cursor_close(cursor);                                            \
        }                                                                      \
        if (txn) {                                                             \
          mdb_txn_abort(txn);                                                  \
        }                                                                      \
        caml_raise_with_arg(*caml_named_value("ocaml_lmdb_error"),             \
                            Val_int(_tmp));                                    \
      }                                                                        \
    }                                                                          \
  } while (0)

CAMLprim value ocaml_lmdb_put_profile(value _env, value _profile)
{
  CAMLparam2(_env, _profile);
  MDB_txn *txn = NULL;
  MDB_env *env = Env_val(_env);
  MDB_dbi dbi;
  MDB_val key, data;
  int rc;

  key.mv_size = strlen(ITHACA_PROFILE_KEY);
  key.mv_data = ITHACA_PROFILE_KEY;
  data.mv_size = caml_string_length(_profile);

  caml_register_generational_global_root(&_profile);
  data.mv_data = (void *)String_val(_profile);

  caml_enter_blocking_section();
  rc = mdb_txn_begin(env, NULL, 0, &txn);
  if (rc == 0)
    rc = mdb_dbi_open(txn, ITHACA_PROFILE, MDB_CREATE, &dbi);
  if (rc == 0)
    rc = mdb_put(txn, dbi, &key, &data, 0);
  if (rc != 0) {
    if (txn)
      mdb_txn_abort(txn);
  } else
    rc = mdb_txn_commit(txn);
  caml_leave_blocking_section();

  caml_remove_generational_global_root(&_profile);

  if (rc != 0)
    caml_raise_with_arg(*caml_named_value("ocaml_lmdb_error"), Val_int(rc));

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lmdb_get_profile(value _env)
{
  CAMLparam1(_env);
  CAMLlocal1(_ans);
  MDB_txn *txn = NULL;
  MDB_env *env = Env_val(_env);
  MDB_dbi dbi;
  MDB_val key, data;
  void *buf = NULL;
  size_t buf_size = 0;
  int rc;

  key.mv_size = strlen(ITHACA_PROFILE_KEY);
  key.mv_data = ITHACA_PROFILE_KEY;

  caml_enter_blocking_section();
  rc = mdb_txn_begin(env, NULL, MDB_RDONLY, &txn);
  if (rc == 0)
    rc = mdb_dbi_open(txn, ITHACA_PROFILE, 0, &dbi);
  if (rc == 0)
    rc = mdb_get(txn, dbi, &key, &data);
  if (rc == 0) {
    buf_size = data.mv_size;
    buf = malloc(buf_size);
    if (!buf)
      rc = ENOMEM;
    else
      memcpy(buf, data.mv_data, buf_size);
  }
  if (txn)
    mdb_txn_commit(txn);
  caml_leave_blocking_section();

  if (rc == MDB_NOTFOUND)
    caml_raise_not_found();
  if (rc != 0)
    caml_raise_with_arg(*caml_named_value("ocaml_lmdb_error"), Val_int(rc));

  _ans = caml_alloc_string(buf_size);
  memcpy(Bytes_val(_ans), buf, buf_size);
  free(buf);

  CAMLreturn(_ans);
}

int db_flags = MDB_CREATE | MDB_INTEGERKEY;

static inline int check_hash_saturation(MDB_cursor *cursor, MDB_txn *txn,
                                        MDB_dbi dbi, uint32_t hash)
{
  MDB_val key;
  MDB_val data;
  int ret;

  key.mv_size = sizeof(hash);
  key.mv_data = &hash;

  ret = mdb_get(txn, dbi, &key, &data);

  if (ret == MDB_NOTFOUND)
    return 0;

  check_error(cursor, txn, ret);

  return 1;
}

static inline uint64_t count_hash_ids(MDB_cursor *cursor, MDB_txn *txn,
                                      uint32_t hash)
{
  MDB_val key;
  MDB_val data;
  uint64_t stored_key;
  uint32_t match;
  uint64_t count = 0;
  uint16_t id_r;
  uint16_t old_id_r = 0;
  int ret;

  key.mv_size = sizeof(stored_key);
  key.mv_data = &stored_key;

  stored_key = ((uint64_t)hash) << 32;

  ret = mdb_cursor_get(cursor, &key, &data, MDB_SET_RANGE);

  while (1) {
    if (ret == MDB_NOTFOUND)
      return count;
    check_error(cursor, txn, ret);
    check_error(cursor, txn,
                mdb_cursor_get(cursor, &key, &data, MDB_GET_CURRENT));
    stored_key = *((uint64_t *)key.mv_data);
    match = stored_key >> 32;
    if (match != hash)
      return count;
    id_r = stored_key >> 16;
    if (count == 0 || id_r != old_id_r)
      count++;
    old_id_r = id_r;
    ret = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
  }
}

static inline void mark_hash_as_saturated(MDB_cursor *cursor, MDB_txn *txn,
                                          MDB_dbi sdbi, uint32_t hash)
{
  MDB_val key;
  MDB_val data;
  uint64_t stored_key;
  uint32_t match;
  int ret;

  key.mv_size = sizeof(stored_key);
  key.mv_data = &stored_key;

  stored_key = ((uint64_t)hash) << 32;

  ret = mdb_cursor_get(cursor, &key, &data, MDB_SET_RANGE);

  while (1) {
    if (ret == MDB_NOTFOUND)
      break;
    check_error(cursor, txn, ret);
    check_error(cursor, txn,
                mdb_cursor_get(cursor, &key, &data, MDB_GET_CURRENT));
    match = *((uint64_t *)key.mv_data) >> 32;
    if (match != hash)
      break;
    check_error(cursor, txn, mdb_cursor_del(cursor, 0));
    ret = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
  }

  key.mv_size = sizeof(hash);
  key.mv_data = &hash;

  data.mv_size = sizeof(hash);
  data.mv_data = &hash;

  check_error(cursor, txn, mdb_put(txn, sdbi, &key, &data, 0));
}

CAMLprim value ocaml_lmdb_put(value _env, value _max, value _hashes)
{
  CAMLparam2(_env, _hashes);
  CAMLlocal1(_data);
  MDB_txn *txn = NULL;
  MDB_env *env = Env_val(_env);
  MDB_dbi dbi, sdbi;
  MDB_val key;
  MDB_val data;
  MDB_cursor *cursor = NULL;
  unsigned int l, i;
  uint64_t max = Long_val(_max);
  uint16_t id_r, pos_r;
  uint32_t id_d, pos_d;
  uint64_t stored_key;
  uint64_t stored_data;
  uint32_t hash;

  key.mv_size = sizeof(stored_key);
  ;
  key.mv_data = &stored_key;

  data.mv_size = sizeof(stored_data);
  data.mv_data = &stored_data;

  check_error(NULL, NULL, mdb_txn_begin(env, NULL, 0, &txn));
  check_error(NULL, txn, mdb_dbi_open(txn, ITHACA_DB, db_flags, &dbi));
  check_error(NULL, txn,
              mdb_dbi_open(txn, ITHACA_SATURATED_DB, db_flags, &sdbi));
  check_error(NULL, txn, mdb_cursor_open(txn, dbi, &cursor));

  for (l = 0; l < Wosize_val(_hashes); l++) {
    hash = Int32_val(Field(Field(_hashes, l), 0));

    if (check_hash_saturation(cursor, txn, sdbi, hash) == 1)
      continue;

    _data = Field(Field(_hashes, l), 1);

    for (i = 0; i < Wosize_val(_data); i++) {
      // data is a record {id_r; pos_r; id_d; pos_d}
      id_r = Int_val(Field(Field(_data, i), 0));
      pos_r = Int_val(Field(Field(_data, i), 1));
      id_d = Int_val(Field(Field(_data, i), 2));
      pos_d = Int_val(Field(Field(_data, i), 3));

      // Pack hash,id_r,pos_r into key
      stored_key = ((uint64_t)pos_r) | (((uint64_t)id_r) << 16) |
                   (((uint64_t)hash) << 32);
      // Pack id_d(32) | pos_d(32) into value
      stored_data = ((uint64_t)pos_d) | (((uint64_t)id_d) << 32);

      check_error(cursor, txn, mdb_cursor_put(cursor, &key, &data, 0));

      if (0 < max && max <= count_hash_ids(cursor, txn, hash)) {
        mark_hash_as_saturated(cursor, txn, sdbi, hash);
        break;
      }
    }
  }

  mdb_cursor_close(cursor);
  cursor = NULL;
  check_error(NULL, NULL, mdb_txn_commit(txn));

  CAMLreturn(Val_unit);
}

static value fetch_values(MDB_cursor *cursor, MDB_txn *txn, value ans,
                          size_t pos)
{
  CAMLparam1(ans);
  CAMLlocal1(tmp);

  uint16_t id_r, pos_r;
  uint32_t id_d, pos_d;
  uint64_t stored;
  MDB_val key, data;

  check_error(cursor, txn,
              mdb_cursor_get(cursor, &key, &data, MDB_GET_CURRENT));

  stored = *((uint64_t *)key.mv_data);
  pos_r = stored;
  id_r = stored >> 16;

  stored = *((uint64_t *)data.mv_data);
  pos_d = stored & 0xFFFFFFFF;
  id_d = stored >> 32;

  // Store record {id_r; pos_r; id_d; pos_d}
  tmp = caml_alloc_tuple(4);

  Store_field(tmp, 0, Val_int(id_r));
  Store_field(tmp, 1, Val_int(pos_r));
  Store_field(tmp, 2, Val_int(id_d));
  Store_field(tmp, 3, Val_int(pos_d));

  Store_field(ans, pos, tmp);

  CAMLreturn(Val_unit);
}

CAMLprim value ocaml_lmdb_get(value _env, value _keys)
{
  CAMLparam2(_env, _keys);
  CAMLlocal2(ans, tmp);
  MDB_txn *txn = NULL;
  MDB_env *env = Env_val(_env);
  MDB_cursor *cursor = NULL;
  MDB_dbi dbi, sdbi;
  MDB_val key;
  MDB_val data;
  uint16_t k, c, n = 0;
  uint32_t match, hash;
  uint64_t initial_key;
  int ret;

  check_error(NULL, NULL, mdb_txn_begin(env, NULL, MDB_RDONLY, &txn));
  check_error(NULL, txn, mdb_dbi_open(txn, ITHACA_DB, db_flags, &dbi));
  check_error(NULL, txn,
              mdb_dbi_open(txn, ITHACA_SATURATED_DB, db_flags, &sdbi));
  check_error(NULL, txn, mdb_cursor_open(txn, dbi, &cursor));

  ans = caml_alloc_tuple(Wosize_val(_keys));

  for (k = 0; k < Wosize_val(_keys); k++) {
    hash = Int32_val(Field(_keys, k));

    if (check_hash_saturation(cursor, txn, sdbi, hash) == 1) {
      Store_field(ans, k, Atom(0));
      continue;
    }

    initial_key = ((uint64_t)hash) << 32;
    key.mv_size = sizeof(initial_key);
    key.mv_data = &initial_key;

    ret = mdb_cursor_get(cursor, &key, &data, MDB_SET_RANGE);

    if (ret == MDB_NOTFOUND) {
      Store_field(ans, k, Atom(0));
      continue;
    }

    check_error(cursor, txn, ret);

    check_error(cursor, txn,
                mdb_cursor_get(cursor, &key, &data, MDB_GET_CURRENT));
    match = *((uint64_t *)key.mv_data) >> 32;
    if (match != hash) {
      Store_field(ans, k, Atom(0));
      continue;
    }

    // Count data
    n = 1;
    while (1) {
      ret = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);

      if (ret == MDB_NOTFOUND)
        break;
      check_error(cursor, txn, ret);

      check_error(cursor, txn,
                  mdb_cursor_get(cursor, &key, &data, MDB_GET_CURRENT));
      match = *((uint64_t *)key.mv_data) >> 32;
      if (match != hash)
        break;

      n++;
    }

    // Rewind cursor
    for (c = n; 0 < c; c--)
      check_error(cursor, txn, mdb_cursor_get(cursor, &key, &data, MDB_PREV));

    tmp = caml_alloc_tuple(n);

    fetch_values(cursor, txn, tmp, 0);

    for (c = 1; c < n; c++) {
      check_error(cursor, txn, mdb_cursor_get(cursor, &key, &data, MDB_NEXT));
      fetch_values(cursor, txn, tmp, c);
    }

    Store_field(ans, k, tmp);
  }

  mdb_cursor_close(cursor);
  cursor = NULL;
  check_error(NULL, NULL, mdb_txn_commit(txn));

  CAMLreturn(ans);
}
