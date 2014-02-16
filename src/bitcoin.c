/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include <glib.h>
#include <openssl/sha.h>
#include <stdio.h>
#include "bitcoin.h"

guint64 var_int(const guint8 *const buf)
{
	if (*buf == 0xfd) return GUINT16_FROM_LE(*(guint16*)(buf+1));
	if (*buf == 0xfe) return GUINT32_FROM_LE(*(guint32*)(buf+1));
	if (*buf == 0xff) return GUINT64_FROM_LE(*(guint64*)(buf+1));
	return *buf;
}

int var_int_len(const guint8 *const buf)
{
	if (*buf == 0xfd) return 3;
	if (*buf == 0xfe) return 5;
	if (*buf == 0xff) return 9;
	return 1;
}

guint32 checksum(const struct msg *const m)
{
	return *(guint32*)dhash(m->payload,GUINT32_FROM_LE(m->length_le));
}

guchar *dhash(const guchar *const d, const gulong n)
{
	return SHA256(SHA256(d,n,NULL),32,NULL);
}

gchar *hex256(const guchar *const in)
{
	static char out[65];

	for (int i=0; i<32; i++) {
		snprintf(out+2*i,3,"%02hhx",in[31-i]);
	}

	return out;
}
