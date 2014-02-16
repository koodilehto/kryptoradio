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
guchar *dhash(const guchar *const d, const gulong n);

/**
 * Returns number of "meaningful bytes" in an inventory item. Rest are
 * ignored when calculating the hash. If this number is -1, this item
 * has no hash (it is not tx or block).
 */
int bitcoin_hashable_length(const struct msg *const m);

/**
 * Calculates inventory hash from given message.
 */
const guchar *const bitcoin_inv_hash(const struct msg *const m);
char *hex256(const guchar *const buf);
bool bitcoin_join(int fd);

#endif /* BITCOIN_H_ */
