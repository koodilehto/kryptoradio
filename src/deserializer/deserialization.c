/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C ../.." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 *
 *  Copyright 2014 Joel Lehtonen
 *  
 *  This program is free software: you can redistribute it and/or
 *  modify it under the terms of the GNU Affero General Public License
 *  as published by the Free Software Foundation, either version 3 of
 *  the License, or (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Affero General Public License for more details.
 *  
 *  You should have received a copy of the GNU Affero General Public
 *  License along with this program.  If not, see
 *  <http://www.gnu.org/licenses/>.
 */

#include <assert.h>
#include <err.h>
#include <stdio.h>
#include <unistd.h>
#include "deserialization.h"

/**
 * Tries to sync the decompressor. This should be called with new
 * input if s->has_sync is false until s->has_sync is true. Do not
 * call this function if the receiver is already in sync or you will
 * lose some input.
 */
static void zlib_try_sync(struct decoder_state *s);

/**
 * Stores received packet for later processing.
 */
void store_packet(struct bitcoin_receive_storage *const st, guint8 *buf, int len);

void deserialize_init(struct decoder_state *s)
{
	s->has_sync = true;
	s->size = 128;
	s->buf = g_malloc(s->size);
	s->zlib.zalloc = Z_NULL;
	s->zlib.zfree = Z_NULL;
	s->zlib.opaque = Z_NULL;
	s->zlib.avail_in = 0;
	s->zlib.next_in = Z_NULL;
	s->zlib.avail_out = s->size;
	s->zlib.next_out = s->buf;
	int ret = inflateInit(&s->zlib);
	if (ret != Z_OK) errx(10,"Unable to initialize decompressor");
}

void deserialize(const int devfd, struct bitcoin_receive_storage *const st, struct decoder_state *s)
{
	guint8 buf[2048];
	s->zlib.avail_in = read(devfd,buf,2048);
	s->zlib.next_in = buf; 

	printf("Got %d bytes\n",s->zlib.avail_in);

	if (s->zlib.avail_in == 0) {
		errx(3,"Unexpected end of bitcoin stream");
	} else if (s->zlib.avail_in == -1) {
		err(3,"Error reading bitcoind stream");
	}

	if (!s->has_sync) {
		// Trying to sync. This reduces the number of bytes in
		// the input buffer
		zlib_try_sync(s);
	}

	while (s->zlib.avail_in) {
		const int before = s->zlib.avail_out;
		const int ret = inflate(&s->zlib, Z_BLOCK);
		if (ret == Z_DATA_ERROR) {
			printf("Sync lost. Waiting for sync.\n");

			// Abandon all pending output
			s->zlib.next_out = s->buf;
			s->zlib.avail_out = s->size;

			// We need to fast-forward to the next
			// probable sync point.
			s->has_sync = false;
			zlib_try_sync(s);
			continue;
		} else if (ret != Z_OK) {
			errx(10,"Decoding error. Error code %d.", ret);
		}

		if (s->zlib.avail_out == 0) {
			// Double output space and come back
			const int zlib_index = s->zlib.next_out - s->buf;
			s->zlib.avail_out += s->size;
			s->size *= 2;
			s->buf = g_realloc(s->buf, s->size);
			// zlib data output pointer must be updated
			// because g_realloc may move the data
			s->zlib.next_out = s->buf + zlib_index;
		} else if (s->zlib.avail_out == before) {
			// We've got a packet
			int packet_len = s->zlib.next_out - s->buf;
			printf("Got a packet of %d bytes\n", packet_len);

			store_packet(st, s->buf, packet_len);

			// Rewind the receive buffer.
			s->zlib.next_out = s->buf;
			s->zlib.avail_out = s->size;
		}
	}
}

static void zlib_try_sync(struct decoder_state *s)
{
	const int ret = inflateSync(&s->zlib);
	if (ret == Z_OK) {
		// Sync
		printf("Got sync!\n");
		s->has_sync = true;
	} else if (ret == Z_DATA_ERROR) {
		// No sync yet, try again with more data.

		// Expecting zlib implementation to consume all input
		// in case of sync error
		assert(s->zlib.avail_in == 0);
	} else {
		// Fatal fail.
		errx(10,"Unable to sync: zlib error %d", ret);
	}
}

void store_packet(struct bitcoin_receive_storage *const st, guint8 *buf, int len)
{
	// TODO
}
