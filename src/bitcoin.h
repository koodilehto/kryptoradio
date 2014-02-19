/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#ifndef BITCOIN_H_
#define BITCOIN_H_

#include <glib.h>
#include <stdbool.h>

struct __attribute__ ((__packed__)) msg {
	guint32 magic;
	char command[12];
	guint32 length_le; // Little-endian!
	guint32 checksum;
	guint8 payload[];
};

// TODO function comments

guint64 var_int(const guint8 *const buf);
gint var_int_len(const guint8 *const buf);
guint32 checksum(const struct msg *const m);

/**
 * Calculates double SHA256 of given data. This is described in
 * Bitcoin Protocol Specification.
 */
guchar *dhash(const guchar *const d, const gulong n, guchar *const md);

/**
 * Returns number of "identifying bytes" in an inventory item. Rest may
 * ignored when calculating the hash.
 */
int bitcoin_hashable_length(const struct msg *const m);

/**
 * Calculates inventory hash from given message using static buffer.
 */
const guchar *const bitcoin_inv_hash(const struct msg *const m);

/**
 * Calculates inventory hash from given message using buffer allocated
 * by the caller. If md is NULL the behaviour is identical to
 * bitcoin_inv_hash().
 */
guchar *bitcoin_inv_hash_buf(const struct msg *const m, guchar *const md);

char *hex256(const guchar *const buf);
bool bitcoin_join(int fd);

/**
 * Returns a new Bitcoin message inventory which uses bitcoin specific
 * hash calculation and equality test. Only data of type `struct msg`
 * should be stored here.
 */
GHashTable *bitcoin_new_inventory();

/**
 * Insert given message to inventory of objects. This function doesn't
 * modify m, but it is not declared as const because glib doesn't have
 * constant pointers to data.
 */
bool bitcoin_inv_insert(GHashTable *inv, struct msg *const m);

#endif /* BITCOIN_H_ */
