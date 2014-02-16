/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include <glib.h>
#include <openssl/sha.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "bitcoin.h"

// Local prototypes
static guint bitcoin_msg_hashtable_hash(gconstpointer key);
static gboolean bitcoin_msg_eq(gconstpointer a, gconstpointer b);

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
	return *(guint32*)dhash(m->payload,GUINT32_FROM_LE(m->length_le));
}

guchar *dhash(const guchar *const d, const gulong n)
{
	return SHA256(SHA256(d,n,NULL),SHA256_DIGEST_LENGTH,NULL);
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
	int hash_end = bitcoin_hashable_length(m);
	return dhash(m->payload,hash_end);
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

GHashTable *bitcoin_new_inventory() {
	return g_hash_table_new(&bitcoin_msg_hashtable_hash,bitcoin_msg_eq);
}

/**
 * Calculates a hash for use in Hash Table storage. NB! This hash is
 * not the same as in Bitcoin Protocol Specification. The risk of
 * collision is larger but is not fatal because GLib hash table
 * handles collisions.
 */
static guint bitcoin_msg_hashtable_hash(gconstpointer key)
{
	// Using checksum as a hash because collisions are not fatal.
	return ((const struct msg *)key)->checksum;
}

/**
 * Compares two messages for equality. It uses optimization where only
 * relevant parts of the message is compared (only headers are used in
 * case of a block)
 */
static gboolean bitcoin_msg_eq(gconstpointer a, gconstpointer b)
{	
	int a_len = bitcoin_hashable_length(a);
	int b_len = bitcoin_hashable_length(b);

	if (a_len != b_len) return FALSE;

	// a_len == b_len in this case
	return 0 == memcmp(a,b,sizeof(struct msg)+a_len);
}
