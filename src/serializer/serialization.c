/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C ../.." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include <stdio.h>
#include <glib.h>
#include <stdbool.h>
#include <unistd.h>
#include <string.h>
#include <err.h>
#include <zlib.h>
#include "serialization.h"

#define PAD_COUNT 100 // If padding is enabled, output this many pads at once

struct encoder_state {
	guint8 *buf;
	int size;
	z_stream zlib;
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
}

static void encode_start(struct encoder_state *s)
{
	s->zlib.avail_out = s->size;
	s->zlib.next_out = s->buf;
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
