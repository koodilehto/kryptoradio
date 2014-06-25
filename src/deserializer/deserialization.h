/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C ../.." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include <glib.h>
#include <zlib.h>

struct decoder_state {
	guint8 *buf;
	int size;
	z_stream zlib;
};

/**
 * Initializes deserializer. This allocates some memory and
 * initializes its internal state.
 */
void deserialize_init(struct decoder_state *s);

/**
 * Processes incoming data. Does not block.
 */
void deserialize(const int devfd, struct decoder_state *s);
