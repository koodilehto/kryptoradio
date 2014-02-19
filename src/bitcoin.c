/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include <err.h>
#include <glib.h>
#include <openssl/sha.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "bitcoin.h"

// Local prototypes
static guint dhash_hash(gconstpointer key);
static gboolean dhash_eq(gconstpointer a, gconstpointer b);

// Local constants
static const guint8 join_message[] = {
	0xF9, 0xBE, 0xB4, 0xD9, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6F,
	0x6E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x64, 0x00, 0x00, 0x00,
	0x35, 0x8D, 0x49, 0x32, 0x62, 0xEA, 0x00, 0x00, 0x01, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x11, 0xB2, 0xD0, 0x50,
	0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3B, 0x2E, 0xB3, 0x5D,
	0x8C, 0xE6, 0x17, 0x65, 0x0F, 0x2F, 0x53, 0x61, 0x74, 0x6F,
	0x73, 0x68, 0x69, 0x3A, 0x30, 0x2E, 0x37, 0x2E, 0x32, 0x2F,
	0xC0, 0x3E, 0x03, 0x00
};

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
	return *(guint32*)dhash(m->payload,GUINT32_FROM_LE(m->length_le),NULL);
}

guchar *dhash(const guchar *const d, const gulong n, guchar *const md)
{
	return SHA256(SHA256(d,n,NULL),SHA256_DIGEST_LENGTH,md);
}

int bitcoin_hashable_length(const struct msg *const m)
{
	if (strcmp(m->command,"block") == 0) {
		// Block hash is calculated only from 6 first fields
		return 4+32+32+4+4+4;
	} else {
		// Use all bytes to calculate the hash
		return GUINT32_FROM_LE(m->length_le);
	}
}

const guchar *const bitcoin_inv_hash(const struct msg *const m)
{
	return bitcoin_inv_hash_buf(m,NULL);
}

guchar *bitcoin_inv_hash_buf(const struct msg *const m, guchar *const md)
{
	int hash_end = bitcoin_hashable_length(m);
	return dhash(m->payload,hash_end,md);
}

gchar *hex256(const guchar *const in)
{
	static char out[2*SHA256_DIGEST_LENGTH+1];

	for (int i=0; i<SHA256_DIGEST_LENGTH; i++) {
		snprintf(out+2*i,3,"%02hhx",in[SHA256_DIGEST_LENGTH-1-i]);
	}

	return out;
}

bool bitcoin_join(int fd)
{
	return write(fd,join_message,sizeof(join_message)) !=
		sizeof(join_message);
}

GHashTable *bitcoin_new_inventory()
{
	return g_hash_table_new_full(&dhash_hash,dhash_eq,free,free);
}

bool bitcoin_inv_insert(GHashTable *inv, struct msg *const m)
{
	// Allocate buffer for key and calculate hash of a message
	guchar* key = malloc(SHA256_DIGEST_LENGTH);
	if (key == NULL) errx(5,"Memory allocation failed");
	bitcoin_inv_hash_buf(m, key);

	// If key is already stored, do not replace old one
	if (g_hash_table_contains(inv,key)) {
		free(key);
		return false;
	}

	// Store message
	g_hash_table_insert(inv,key,m);
	return true;
}

/**
 * Calculates a hash for use internally in Hash Table storage. This
 * hash should not be used outside g_hash_table.
 */
static guint dhash_hash(gconstpointer key)
{
	// Truncate first bytes of the key because the key is already
	// a hash.
	return *(guint*)key;
}

/**
 * Compares two keys for equality. It is used internally in Hash Table
 * storage.
 */
static gboolean dhash_eq(gconstpointer a, gconstpointer b)
{	
	// The keys are already hashes so comparing them byte-by-byte.
	return memcmp(a,b,SHA256_DIGEST_LENGTH) == 0;
}
