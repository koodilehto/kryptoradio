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
unsigned char *dhash(const guchar *const d, const gulong n);
char *hex256(const guchar *const buf);
bool bitcoin_join(int fd);

#endif /* BITCOIN_H_ */
