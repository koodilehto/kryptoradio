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

#include <stdio.h>
#include <glib.h>
#include <stdbool.h>
#include <unistd.h>
#include <string.h>
#include <err.h>
#include <zlib.h>
#include <time.h>
#include "serialization.h"

#define PAD_COUNT 100 // If padding is enabled, output this many pads at once
#define RESET_INTERVAL 15 // Perform Z_FULL_FLUSH after this many seconds

struct encoder_state {
	guint8 *buf;
	int size;
	z_stream zlib;
	struct timespec next_reset;
};

// Prototypes
int secp256k1_ecdsa_sign(const unsigned char *msg, int msglen,
                         unsigned char *sig, int *siglen,
                         const unsigned char *seckey,
                         const unsigned char *nonce);
static void encoder_state_init(struct encoder_state *s);
static void encode_start(struct encoder_state *s);
static void encode(struct encoder_state *s, void *src, const int n);
static int encode_end(struct encoder_state *s);
static void encode_flush(struct encoder_state *const s, const int flush);

/**
 * Does a "blind full flush" which means that if there is already a
 * sync flush in the stream, this converts it to a full flush. Because
 * they differ only in the sense that full flush doesn't use old
 * dictionary, it is OK to do this kind of stream rewinding trick. You
 * must have done Z_SYNC_FLUSH or Z_FULL_FLUSH before calling this or
 * this will seriously damage the stream.
 */
static void encode_blind_full_flush(struct encoder_state *const s);

/**
 * Does a "blind full flush" if more than RESET_INTERVAL has elapsed
 * after last reset. (see comments in encode_blind_full_flush() for
 * more details. */
static void encode_maybe_full_flush(struct encoder_state *const s);

bool serialize(const int devfd, struct bitcoin_storage *const st, bool serial_pad)
{
	static struct encoder_state s;
	static bool first_run = true;
	static int buf_left = 0; // Bytes left to send
	static int buf_i = 0; // Bytes position of next unsent byte

	if (first_run) {
		encoder_state_init(&s);
		first_run = false;
	}

	gint queued = heap_size(&st->send_queue);

	if (!buf_left && queued) {
		// Try to fill send buffer
		struct msg *m = bitcoin_dequeue(st);

		// Should not happen
		if (m == NULL) errx(6,"Send queue handling error");

		// Do not retransmit if it is already sent.
		if (m->sent) {
			printf("Already sent %s %s, skipping\n",
			       bitcoin_type_str(m),
			       hex256(bitcoin_inv_hash(m)));
			return true;
		}

		// Mark message as sent
		m->sent = true;

		// Calculate signature. FIXME: Include type in calculation!
		
		// Bitcoin message header in unidirectional
		// transfer. The signature is used to verify the
		// transmitter. Transactions have their own signature
		// inside message body, too.
		
		unsigned char sig[72];
		int siglen;
		secp256k1_ecdsa_sign(m->payload,m->length,sig,&siglen,NULL,NULL);

		encode_start(&s);
		encode(&s,&siglen,1); // FIXME doesn't work on big endian archs
		encode(&s,&sig,siglen);
		encode(&s,&m->type,1); // FIXME doesn't work on big endian archs
		encode(&s,m->payload,m->length);

		// Finishing encoding and updating send buffer
		buf_left = encode_end(&s);
		buf_i = 0;

		// Debugging
		char height_buf[20] = "";
		if (m->height != UNCONFIRMED) {
			snprintf(height_buf,sizeof(height_buf),
				 " @ %d",m->height);
		}
		printf("Sending %s %s%s, %d bytes, %d items left\n",
		       bitcoin_type_str(m),
		       hex256(bitcoin_inv_hash(m)),
		       height_buf,
		       m->length,
		       queued);
	}

	if (buf_left) {
		// Consume buffer as much as possible
		const int wrote = write(devfd, s.buf+buf_i, buf_left);
		if (wrote == 0) {
			errx(3,"Weird behaviour on serial port");
		} else if (wrote == -1) {
			err(4,"Unable to write to serial port");
		}
		buf_i += wrote;
		buf_left -= wrote;
	} else {
		if (!serial_pad) {
			// All is sent, do not come back until there
			// is more data availableq
			printf("Serial device idle\n");
			return false;
		}
		// Send padding and go back to waiting loop.
		guint8 buf[PAD_COUNT*5];
		int i = 0;
		while (i < sizeof(buf)) {
			// Z_SYNC_FLUSH is 5 bytes long:
			buf[i++] = 0x00;
			buf[i++] = 0x00;
			buf[i++] = 0x00;
			buf[i++] = 0xFF;
			buf[i++] = 0xFF;
		}

		const int ret = write(devfd,buf,sizeof(buf));
		if (ret < 1) err(4,"Unable to write to serial port");
		if (ret != sizeof(buf)) err(10,"Too large padding buffer or non-linuxy system");
		printf("Sending %d bytes of padding\n",ret);
	}
	return true;
}

/**
 * Dummy placeholder function. Original one creates an ECDSA signature.
 *
 * Returns: 1: signature created
 *          0: nonce invalid, try another one
 * In:      msg: the message being signed
 *          msglen: the length of the message being signed
 *          seckey: pointer to a 32-byte secret key (assumed to be valid)
 *          nonce: pointer to a 32-byte nonce (generated with a cryptographic PRNG)
 * Out:     sig: pointer to a 72-byte array where the signature will be placed.
 *          siglen: pointer to an int, which will be updated to the signature length (<=72).
 */
int secp256k1_ecdsa_sign(const unsigned char *msg, int msglen,
                         unsigned char *sig, int *siglen,
                         const unsigned char *seckey,
                         const unsigned char *nonce)
{
	const char dummy[] = "signature";
	strcpy((char*)sig,dummy);
	*siglen = sizeof(dummy);
	return 1;
}

static void encoder_state_init(struct encoder_state *s)
{
	s->size = 128;
	s->buf = g_malloc(s->size);
	s->zlib.zalloc = Z_NULL;
	s->zlib.zfree = Z_NULL;
	s->zlib.opaque = Z_NULL;
	int ret = deflateInit(&s->zlib,Z_BEST_COMPRESSION);
	if (ret != Z_OK) errx(10,"Unable to initialize compressor");
	s->next_reset.tv_sec = 0;
	s->next_reset.tv_nsec = 0;
}

static void encode_start(struct encoder_state *s)
{
	s->zlib.avail_out = s->size;
	s->zlib.next_out = s->buf;
	encode_maybe_full_flush(s);
}

static void encode(struct encoder_state *s, void *src, const int n)
{
	s->zlib.next_in = src;
	s->zlib.avail_in = n;
	encode_flush(s, Z_NO_FLUSH);
}

static int encode_end(struct encoder_state *s)
{
	// Perform sync flush. It allows to transmit whole packet and
	// also provides a marker (00 00 FF FF) between packets.
	encode_flush(s, Z_SYNC_FLUSH);
	return s->size - s->zlib.avail_out;
}

static void encode_flush(struct encoder_state *const s, const int flush)
{
	while (true) {
		int ret = deflate(&s->zlib, flush);
		if (ret != Z_OK) errx(10,"Error in compression");

		if (s->zlib.avail_out) {
			// Everything fits
			return;
		} else {
			// Double output space and come back
			const int zlib_index = s->zlib.next_out - s->buf;
			s->zlib.avail_out += s->size;
			s->size *= 2;
			s->buf = g_realloc(s->buf, s->size);
			// zlib data output pointer must be updated
			// because g_realloc may move the data
			s->zlib.next_out = s->buf + zlib_index;
		}
	}
}

static void encode_blind_full_flush(struct encoder_state *const s)
{
	encode_flush(s,Z_FULL_FLUSH);
	// When there are no bits pending, Z_FULL_FLUSH is 5 bytes
	s->zlib.next_out -= 5;
	s->zlib.avail_out += 5;
}

static void encode_maybe_full_flush(struct encoder_state *const s)
{
	struct timespec now;
	// FIXME: This is a Linux specific clock source. Use feature
	// test macro.
	if (clock_gettime(CLOCK_MONOTONIC_COARSE, &now)) {
		err(10,"Unable to get monotonic time");
	}

	// Do nothing if interval not yet reached
	if (now.tv_sec < s->next_reset.tv_sec) return;
	if (now.tv_sec == s->next_reset.tv_sec &&
	    now.tv_nsec < s->next_reset.tv_nsec) return;

	// Set the time for next sync
	now.tv_sec += RESET_INTERVAL;
	s->next_reset = now;

	printf("Doing full flush\n");
	encode_blind_full_flush(s);
}
