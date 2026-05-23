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

#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/misc.h>
#include <caml/mlvalues.h>

#include <stdint.h>

static inline int16_t bswap_16(int16_t x)
{
  return ((((x) >> 8) & 0xff) | (((x) & 0xff) << 8));
}

#define u8tof(x) (((double)x - INT8_MAX) / INT8_MAX)
#define get_u8(src, offset, nc, c, i)                                          \
  u8tof(((uint8_t *)src)[offset + i * nc + c])
#define s16tof(x) (((double)x) / INT16_MAX)
#define get_s16le(src, offset, nc, c, i)                                       \
  s16tof(((int16_t *)src)[offset / 2 + i * nc + c])
#define get_s16be(src, offset, nc, c, i)                                       \
  s16tof(bswap_16(((int16_t *)src)[offset / 2 + i * nc + c]))

CAMLprim value caml_float_pcm_of_u8_native(value _src, value _offset,
                                           value _dst, value _dst_off,
                                           value _length)
{
  CAMLparam2(_src, _dst);
  CAMLlocal1(dstc);
  const char *src = String_val(_src);
  int offset = Int_val(_offset);
  int len = Int_val(_length);
  int dst_off = Int_val(_dst_off);
  int nc = Wosize_val(_dst);
  int dst_len;
  int i, c;

  if (nc == 0)
    CAMLreturn(Val_unit);
  dst_len = Wosize_val(Field(_dst, 0)) / Double_wosize;

  if ((size_t)offset + (size_t)len * nc > caml_string_length(_src))
    caml_invalid_argument("convert_native: input buffer too small");

  if (dst_off + len > dst_len)
    caml_invalid_argument("convert_native: output buffer too small");

  for (c = 0; c < nc; c++) {
    dstc = Field(_dst, c);
    for (i = 0; i < len; i++) {
      Store_double_field(dstc, dst_off + i, get_u8(src, offset, nc, c, i));
    }
  }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_float_pcm_of_u8_byte(value *argv, int argn)
{
  (void)argn;
  return caml_float_pcm_of_u8_native(argv[0], argv[1], argv[2], argv[3],
                                     argv[4]);
}

CAMLprim value caml_float_pcm_convert_s16_native(value _src, value _offset,
                                                 value _dst, value _dst_off,
                                                 value _length, value _le)
{
  CAMLparam2(_src, _dst);
  CAMLlocal1(dstc);
  const char *src = String_val(_src);
  int offset = Int_val(_offset);
  int len = Int_val(_length);
  int dst_off = Int_val(_dst_off);
  int nc = Wosize_val(_dst);
  int dst_len;
  int i, c;

  if (nc == 0)
    CAMLreturn(Val_unit);
  dst_len = Wosize_val(Field(_dst, 0)) / Double_wosize;

  if ((size_t)offset + (size_t)len * nc * 2 > caml_string_length(_src))
    caml_invalid_argument("convert_native: input buffer too small");

  if (dst_off + len > dst_len)
    caml_invalid_argument("convert_native: output buffer too small");

  if (_le == Val_true)
    for (c = 0; c < nc; c++) {
      dstc = Field(_dst, c);
      for (i = 0; i < len; i++)
        Store_double_field(dstc, dst_off + i, get_s16le(src, offset, nc, c, i));
    }
  else
    for (c = 0; c < nc; c++) {
      dstc = Field(_dst, c);
      for (i = 0; i < len; i++)
        Store_double_field(dstc, dst_off + i, get_s16be(src, offset, nc, c, i));
    }

  CAMLreturn(Val_unit);
}

CAMLprim value caml_float_pcm_convert_s16le_native(value _src, value _offset,
                                                   value _dst, value _dst_off,
                                                   value _length, value _le)
{
  return caml_float_pcm_convert_s16_native(_src, _offset, _dst, _dst_off,
                                           _length, _le);
}

CAMLprim value caml_float_pcm_convert_s16le_byte(value *argv, int argn)
{
  (void)argn;
  return caml_float_pcm_convert_s16le_native(argv[0], argv[1], argv[2], argv[3],
                                             argv[4], argv[5]);
}
