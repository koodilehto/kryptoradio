/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include <stdio.h>
#include <glib.h>
#include <stdbool.h>
#include <unistd.h>
#include <string.h>
#include <err.h>
#include "serialization.h"

#define SERIAL_ESC 0xC0
#define SERIAL_LITERAL 0x00
#define SERIAL_START 0x01

struct encoder_state {
	guint8 *p;
	guint8 *start;
};

// Prototypes
int secp256k1_ecdsa_sign(const unsigned char *msg, int msglen,
                         unsigned char *sig, int *siglen,
                         const unsigned char *seckey,
                         const unsigned char *nonce);
void encode_init(struct encoder_state *s, guint8 *buf, bool escaped);
void encode(struct encoder_state *s, const void *src, const int n);
int encode_end(const struct encoder_state *const s);

bool serialize(const int devfd, struct bitcoin_storage *const st, bool serial_pad)
{
	static guint8 *buf_start = NULL; // Start of send buffer
	static guint8 *buf_p = NULL; // Pointer to next unsent byte
	static int buf_allocated = 0; // Bytes allocated for buffer
	static int buf_left = 0; // Bytes left to send
	static bool escaped = false; // Minor optimization when using
				     // continuous transmit mode

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

		/* Pessimistic estimate: Message starts with a header
		 * (2 bytes) and payload has the following: signature
		 * length (1) + maximum signature length (72) + type
		 * (1) + content. Every character triggers escape,
		 * thus doubling the space needed. */
		const int max_buffer_len = 2+2*(1+72+1+m->length);
		// Reallocate buffer
		if (buf_allocated < max_buffer_len) {
			buf_start = g_realloc(buf_start,max_buffer_len);
			buf_allocated = max_buffer_len;
		}

		// Calculate signature. FIXME: Include type in calculation!
		
		// Bitcoin message header in unidirectional
		// transfer. The signature is used to verify the
		// transmitter. Transactions have their own signature
		// inside message body, too.
		
		unsigned char sig[72];
		int siglen;
		secp256k1_ecdsa_sign(m->payload,m->length,sig,&siglen,NULL,NULL);

		struct encoder_state s;
		encode_init(&s,buf_start,escaped);
		encode(&s,&siglen,1); // FIXME doesn't work on big endian archs
		encode(&s,&sig,siglen);
		encode(&s,&m->type,1); // FIXME doesn't work on big endian archs
		encode(&s,m->payload,m->length);

		// Finishing encoding and updating buffer
		buf_left = encode_end(&s);
		buf_p = buf_start;
		escaped = false;

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
		const int wrote = write(devfd,buf_p,buf_left);
		if (wrote == 0) {
			errx(3,"Weird behaviour on serial port");
		} else if (wrote == -1) {
			err(4,"Unable to write to serial port");
		}
		buf_p += wrote;
		buf_left -= wrote;
	} else {
		if (!serial_pad) {
			// Send escape character after all data has been sent
			if (!escaped) {
				const char esc = SERIAL_ESC;
				const int ret = write(devfd,&esc,1);
				if (ret < 1) err(4,"Unable to write to serial port");
				printf("Serial device idle\n");
				escaped = true;
			}
			return false;
		}
		// Send padding and go back to waiting loop
		const char buf[1024];
		memset(&buf,SERIAL_ESC,sizeof(buf));
		const int ret = write(devfd,buf,sizeof(buf));
		if (ret < 1) err(4,"Unable to write to serial port");
		printf("Sending %d bytes of padding\n",ret);
		escaped = true;
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

void encode_init(struct encoder_state *s, guint8 *buf, bool escaped)
{
	s->start = buf;
	s->p = buf;
	if (!escaped) {
		*s->p++ = SERIAL_ESC;
	}
	*s->p++ = SERIAL_START;
}

void encode(struct encoder_state *s, const void *const src, const int n)
{
	for (int i=0; i<n; i++) {
		guint8 byte = ((const guint8*)src)[i];
		*s->p++ = byte;
		if (byte == SERIAL_ESC) *s->p++ = SERIAL_LITERAL;
	}
}

int encode_end(const struct encoder_state *const s)
{
	return s->p-s->start;
}
