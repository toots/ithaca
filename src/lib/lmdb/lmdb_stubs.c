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
#include <stdlib.h>
#include <string.h>

#define ITHACA_DB "db"
#define ITHACA_SATURATED_DB "saturated"
#define ITHACA_COUNTS_DB "counts"
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

static const int db_flags = MDB_CREATE | MDB_INTEGERKEY;

static void raise_lmdb_error(int rc)
{
  if (rc == ENOMEM)
    caml_raise_out_of_memory();
  caml_raise_with_arg(*caml_named_value("ocaml_lmdb_error"), Val_int(rc));
}

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
    raise_lmdb_error(rc);

  mdb_env_set_mapsize(env, map_size);
  mdb_env_set_maxdbs(env, 4);
  mdb_env_set_maxreaders(env, 4096);

  rc = mdb_env_open(env, String_val(file), MDB_NOTLS | MDB_NOSUBDIR, 0755);
  if (rc != 0) {
    mdb_env_close(env);
    raise_lmdb_error(rc);
  }

  wrapper = caml_alloc_custom(&lmdb_env_ops, sizeof(MDB_env *), 0, 1);
  *((MDB_env **)Data_custom_val(wrapper)) = env;

  CAMLreturn(wrapper);
}

CAMLprim value ocaml_lmdb_put_profile(value _env, value _profile)
{
  CAMLparam2(_env, _profile);
  MDB_txn *txn = NULL;
  MDB_env *env = Env_val(_env);
  MDB_dbi dbi;
  MDB_val key, data;
  size_t profile_len = caml_string_length(_profile);
  char *profile = malloc(profile_len);
  int rc;

  if (!profile)
    caml_raise_out_of_memory();
  memcpy(profile, String_val(_profile), profile_len);

  key.mv_size = strlen(ITHACA_PROFILE_KEY);
  key.mv_data = ITHACA_PROFILE_KEY;
  data.mv_size = profile_len;
  data.mv_data = profile;

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

  free(profile);

  if (rc != 0)
    raise_lmdb_error(rc);

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
  if (txn) {
    if (rc == 0 || rc == MDB_NOTFOUND)
      mdb_txn_commit(txn);
    else
      mdb_txn_abort(txn);
  }
  caml_leave_blocking_section();

  if (rc == MDB_NOTFOUND)
    caml_raise_not_found();
  if (rc != 0)
    raise_lmdb_error(rc);

  _ans = caml_alloc_string(buf_size);
  memcpy(Bytes_val(_ans), buf, buf_size);
  free(buf);

  CAMLreturn(_ans);
}

/* The helpers below run inside blocking sections: they must not raise and
 * report failures through their return code instead. */

static int check_hash_saturation(MDB_txn *txn, MDB_dbi sdbi, uint32_t hash,
                                 int *saturated)
{
  MDB_val key, data;
  int rc;

  key.mv_size = sizeof(hash);
  key.mv_data = &hash;

  rc = mdb_get(txn, sdbi, &key, &data);

  if (rc == MDB_NOTFOUND) {
    *saturated = 0;
    return 0;
  }
  if (rc == 0)
    *saturated = 1;
  return rc;
}

/* Per-hash running totals, one record per hash in ITHACA_COUNTS_DB:
 * value_words[0] = total stored entries, value_words[1] = number of put
 * calls (files) that stored this hash. Each put stores a file's entries
 * for a hash under a single id, so the file count matches the distinct-id
 * count that saturation is based on, without scanning the hash's entries. */
static int update_hash_counts(MDB_txn *txn, MDB_dbi cdbi, uint32_t hash,
                              uint64_t added_entries, uint64_t *n_entries,
                              uint64_t *n_files)
{
  MDB_val key, data;
  uint64_t value_words[2] = {0, 0};
  int rc;

  key.mv_size = sizeof(hash);
  key.mv_data = &hash;

  rc = mdb_get(txn, cdbi, &key, &data);
  if (rc == 0 && data.mv_size == sizeof(value_words))
    memcpy(value_words, data.mv_data, sizeof(value_words));
  else if (rc != 0 && rc != MDB_NOTFOUND)
    return rc;

  value_words[0] += added_entries;
  value_words[1] += 1;
  *n_entries = value_words[0];
  *n_files = value_words[1];

  data.mv_size = sizeof(value_words);
  data.mv_data = value_words;

  return mdb_put(txn, cdbi, &key, &data, 0);
}

static int mark_hash_as_saturated(MDB_cursor *cursor, MDB_txn *txn,
                                  MDB_dbi sdbi, MDB_dbi cdbi, uint32_t hash)
{
  MDB_val key, data;
  uint64_t stored_key = ((uint64_t)hash) << 32;
  int rc;

  key.mv_size = sizeof(stored_key);
  key.mv_data = &stored_key;

  rc = mdb_cursor_get(cursor, &key, &data, MDB_SET_RANGE);

  while (rc == 0) {
    if ((uint32_t)(*((uint64_t *)key.mv_data) >> 32) != hash)
      break;
    rc = mdb_cursor_del(cursor, 0);
    if (rc != 0)
      return rc;
    rc = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
  }
  if (rc != 0 && rc != MDB_NOTFOUND)
    return rc;

  key.mv_size = sizeof(hash);
  key.mv_data = &hash;

  rc = mdb_del(txn, cdbi, &key, NULL);
  if (rc != 0 && rc != MDB_NOTFOUND)
    return rc;

  data.mv_size = sizeof(hash);
  data.mv_data = &hash;

  return mdb_put(txn, sdbi, &key, &data, 0);
}

CAMLprim value ocaml_lmdb_put(value _env, value _max, value _max_entries,
                              value _hashes)
{
  CAMLparam2(_env, _hashes);
  CAMLlocal1(_data);
  MDB_txn *txn = NULL;
  MDB_cursor *cursor = NULL;
  MDB_env *env = Env_val(_env);
  MDB_dbi dbi, sdbi, cdbi;
  MDB_val key, data;
  uint64_t max = Long_val(_max);
  uint64_t max_entries = Long_val(_max_entries);
  size_t hash_count = Wosize_val(_hashes);
  size_t total_entries = 0;
  size_t l, i, pos;
  uint16_t id_r, pos_r, bin;
  uint32_t id_d, pos_d;
  uint64_t value_words[2];
  int rc = 0;

  for (l = 0; l < hash_count; l++)
    total_entries += Wosize_val(Field(Field(_hashes, l), 1));

  uint32_t *hashes = malloc(hash_count * sizeof(uint32_t));
  size_t *counts = malloc(hash_count * sizeof(size_t));
  uint64_t *stored_keys = malloc(total_entries * sizeof(uint64_t));
  uint64_t *stored_datas = malloc(total_entries * sizeof(uint64_t));
  uint64_t *stored_bins = malloc(total_entries * sizeof(uint64_t));

  if ((hash_count && (!hashes || !counts)) ||
      (total_entries && (!stored_keys || !stored_datas || !stored_bins))) {
    free(hashes);
    free(counts);
    free(stored_keys);
    free(stored_datas);
    free(stored_bins);
    caml_raise_out_of_memory();
  }

  /* Copy everything out of the OCaml heap so the runtime lock can be
   * released for the duration of the write transaction. */
  pos = 0;
  for (l = 0; l < hash_count; l++) {
    uint32_t hash = Long_val(Field(Field(_hashes, l), 0));
    hashes[l] = hash;
    _data = Field(Field(_hashes, l), 1);
    counts[l] = Wosize_val(_data);

    for (i = 0; i < counts[l]; i++) {
      // data is a record {id_r; pos_r; id_d; pos_d; bin}
      id_r = Int_val(Field(Field(_data, i), 0));
      pos_r = Int_val(Field(Field(_data, i), 1));
      id_d = Int_val(Field(Field(_data, i), 2));
      pos_d = Int_val(Field(Field(_data, i), 3));
      bin = Int_val(Field(Field(_data, i), 4));

      // Pack hash,id_r,pos_r into key
      stored_keys[pos] = ((uint64_t)pos_r) | (((uint64_t)id_r) << 16) |
                         (((uint64_t)hash) << 32);
      // Pack id_d(32) | pos_d(32) into the first value word, the anchor
      // bin into the second
      stored_datas[pos] = ((uint64_t)pos_d) | (((uint64_t)id_d) << 32);
      stored_bins[pos] = bin;
      pos++;
    }
  }

  caml_enter_blocking_section();

  rc = mdb_txn_begin(env, NULL, 0, &txn);
  if (rc == 0)
    rc = mdb_dbi_open(txn, ITHACA_DB, db_flags, &dbi);
  if (rc == 0)
    rc = mdb_dbi_open(txn, ITHACA_SATURATED_DB, db_flags, &sdbi);
  if (rc == 0)
    rc = mdb_dbi_open(txn, ITHACA_COUNTS_DB, db_flags, &cdbi);
  if (rc == 0)
    rc = mdb_cursor_open(txn, dbi, &cursor);

  pos = 0;
  for (l = 0; rc == 0 && l < hash_count; l++) {
    int saturated = 0;

    rc = check_hash_saturation(txn, sdbi, hashes[l], &saturated);
    if (rc != 0 || saturated) {
      pos += counts[l];
      continue;
    }

    for (i = 0; rc == 0 && i < counts[l]; i++) {
      key.mv_size = sizeof(uint64_t);
      key.mv_data = &stored_keys[pos + i];
      value_words[0] = stored_datas[pos + i];
      value_words[1] = stored_bins[pos + i];
      data.mv_size = sizeof(value_words);
      data.mv_data = value_words;

      rc = mdb_cursor_put(cursor, &key, &data, 0);
    }

    /* A hash present in too many distinct tracks, or holding too many
     * entries overall, is non-discriminative: drop all its entries and
     * mark it dead so it is never stored or looked up again. The running
     * totals live in ITHACA_COUNTS_DB so this is O(1) per hash instead of
     * a scan of the hash's stored entries. */
    if (rc == 0 && (0 < max || 0 < max_entries)) {
      uint64_t n_entries, n_files;
      rc = update_hash_counts(txn, cdbi, hashes[l], counts[l], &n_entries,
                              &n_files);
      if (rc == 0 && ((0 < max && max <= n_files) ||
                      (0 < max_entries && max_entries <= n_entries)))
        rc = mark_hash_as_saturated(cursor, txn, sdbi, cdbi, hashes[l]);
    }
    pos += counts[l];
  }

  if (cursor)
    mdb_cursor_close(cursor);
  if (txn) {
    if (rc == 0)
      rc = mdb_txn_commit(txn);
    else
      mdb_txn_abort(txn);
  }

  caml_leave_blocking_section();

  free(hashes);
  free(counts);
  free(stored_keys);
  free(stored_datas);
  free(stored_bins);

  if (rc != 0)
    raise_lmdb_error(rc);

  CAMLreturn(Val_unit);
}

typedef struct {
  uint64_t key;
  uint64_t data;
  uint64_t bin;
} stored_entry;

CAMLprim value ocaml_lmdb_get(value _env, value _keys, value _max_entries)
{
  CAMLparam3(_env, _keys, _max_entries);
  CAMLlocal3(ans, tmp, entry);
  MDB_txn *txn = NULL;
  MDB_env *env = Env_val(_env);
  MDB_cursor *cursor = NULL;
  MDB_dbi dbi, sdbi;
  MDB_val key, data;
  size_t key_count = Wosize_val(_keys);
  size_t entries_len = 0, entries_cap = 1024;
  size_t max_entries = Long_val(_max_entries);
  size_t k, c, pos;
  int rc = 0;

  uint32_t *hashes = malloc(key_count * sizeof(uint32_t));
  size_t *counts = calloc(key_count, sizeof(size_t));
  stored_entry *entries = malloc(entries_cap * sizeof(stored_entry));

  if ((key_count && (!hashes || !counts)) || !entries) {
    free(hashes);
    free(counts);
    free(entries);
    caml_raise_out_of_memory();
  }

  for (k = 0; k < key_count; k++)
    hashes[k] = Long_val(Field(_keys, k));

  caml_enter_blocking_section();

  rc = mdb_txn_begin(env, NULL, MDB_RDONLY, &txn);
  if (rc == 0)
    rc = mdb_dbi_open(txn, ITHACA_DB, db_flags, &dbi);
  if (rc == 0)
    rc = mdb_dbi_open(txn, ITHACA_SATURATED_DB, db_flags, &sdbi);
  if (rc == 0)
    rc = mdb_cursor_open(txn, dbi, &cursor);

  for (k = 0; rc == 0 && k < key_count; k++) {
    uint64_t initial_key = ((uint64_t)hashes[k]) << 32;
    int saturated = 0;
    int ret;

    rc = check_hash_saturation(txn, sdbi, hashes[k], &saturated);
    if (rc != 0 || saturated)
      continue;

    key.mv_size = sizeof(initial_key);
    key.mv_data = &initial_key;

    ret = mdb_cursor_get(cursor, &key, &data, MDB_SET_RANGE);

    /* Skip hashes with more than [max_entries] entries: a geometry shared
     * across that many stored positions is non-discriminative and, left in,
     * makes the per-frame vote counting blow up. 0 disables the cap. */
    size_t start = entries_len;

    while (ret == 0) {
      uint64_t stored_key = *((uint64_t *)key.mv_data);
      if ((uint32_t)(stored_key >> 32) != hashes[k])
        break;
      if (max_entries > 0 && counts[k] >= max_entries) {
        entries_len = start;
        counts[k] = 0;
        break;
      }
      if (entries_len == entries_cap) {
        entries_cap *= 2;
        stored_entry *grown =
            realloc(entries, entries_cap * sizeof(stored_entry));
        if (!grown) {
          rc = ENOMEM;
          break;
        }
        entries = grown;
      }
      entries[entries_len].key = stored_key;
      memcpy(&entries[entries_len].data, data.mv_data, sizeof(uint64_t));
      entries[entries_len].bin = 0;
      if (2 * sizeof(uint64_t) <= data.mv_size)
        memcpy(&entries[entries_len].bin,
               (char *)data.mv_data + sizeof(uint64_t), sizeof(uint64_t));
      entries_len++;
      counts[k]++;
      ret = mdb_cursor_get(cursor, &key, &data, MDB_NEXT);
    }
    if (rc == 0 && ret != 0 && ret != MDB_NOTFOUND)
      rc = ret;
  }

  if (cursor)
    mdb_cursor_close(cursor);
  if (txn) {
    if (rc == 0)
      rc = mdb_txn_commit(txn);
    else
      mdb_txn_abort(txn);
  }

  caml_leave_blocking_section();

  if (rc != 0) {
    free(hashes);
    free(counts);
    free(entries);
    raise_lmdb_error(rc);
  }

  ans = caml_alloc(key_count, 0);

  pos = 0;
  for (k = 0; k < key_count; k++) {
    if (counts[k] == 0) {
      Store_field(ans, k, Atom(0));
      continue;
    }

    tmp = caml_alloc(counts[k], 0);
    Store_field(ans, k, tmp);

    for (c = 0; c < counts[k]; c++, pos++) {
      uint16_t pos_r = entries[pos].key;
      uint16_t id_r = entries[pos].key >> 16;
      uint32_t pos_d = entries[pos].data & 0xFFFFFFFF;
      uint32_t id_d = entries[pos].data >> 32;
      uint16_t bin = entries[pos].bin;

      // Store record {id_r; pos_r; id_d; pos_d; bin}
      entry = caml_alloc_tuple(5);
      Store_field(entry, 0, Val_int(id_r));
      Store_field(entry, 1, Val_int(pos_r));
      Store_field(entry, 2, Val_int(id_d));
      Store_field(entry, 3, Val_int(pos_d));
      Store_field(entry, 4, Val_int(bin));

      Store_field(tmp, c, entry);
    }
  }

  free(hashes);
  free(counts);
  free(entries);

  CAMLreturn(ans);
}
