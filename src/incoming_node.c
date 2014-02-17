/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include <err.h>
#include <glib.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "bitcoin.h"
#include "log.h"

// Processes messages having this format:
// https://en.bitcoin.it/wiki/Protocol_specification#Message_structure
void incoming_node_data(const int fd, GHashTable *const inv)
{
	static struct msg *buf = NULL; // For storing the message payload
	static int buf_allocated = 0;
	static int buf_pos = 0;
	static int buf_left = sizeof(struct msg);

	// Reallocate buffer only if it is too small.
	if (buf_allocated < buf_pos+buf_left) {
		buf = realloc(buf,buf_pos+buf_left);
		if (buf == NULL) errx(5,"Memory allocation failed");
	}

	const int got = read(fd,(void*)buf+buf_pos,buf_left);
	if (got == 0) {
		errx(3,"Unexpected end of bitcoind stream");
	} else if (got == -1) {
		err(3,"Error reading bitcoind stream");
	}
	buf_pos += got;
	buf_left -= got;

	// If not everything is received yet, come back later
	if (buf_left > 0) return;

	// Swapping byte ordering of length
	const guint32 payload_len = GUINT32_FROM_LE(buf->length_le);

	if (buf_pos == sizeof(struct msg) && payload_len != 0) {
		// Header received. To continue reading in next pass,
		// update the message length.
		buf_left = payload_len;
	} else {
		// All payload received. Process content.
		if (checksum(buf) != buf->checksum) {
			errx(3,"Checksum error. Probably we got out of sync.");
		}

		// Store to flat files
		log_msg(buf);

		// Message valid, parsing it
		if (strcmp(buf->command,"inv") == 0) {
			// Requesting transactions and blocks from a peer.

			// Payload in getdata request is identical to
			// inv and checksum is not affected because it
			// is calculated from payload only. We just
			// need to alter command name and send the
			// whole blob back to sender.

			// Because the command name is shorter in inv
			// than in getdata, we may just write over "inv"
			// and trailing null bytes will be fine.
			strcpy(buf->command,"getdata");

			// INFO: buf_pos has the total length (header+payload)
			// TODO: write() may block and become a bottleneck.
			if (write(fd,buf,buf_pos) != buf_pos) {
				err(2,"Sending of getdata has failed");
			}
		} else if (strcmp(buf->command,"tx") == 0 ||
			   strcmp(buf->command,"block") == 0 )
		{
			if (g_hash_table_contains(inv,buf)) {
				// If already received, do not do anything
				warnx("Protocol quirk: duplicate %s %s",
				      buf->command,
				      hex256(bitcoin_inv_hash(buf)));
			} else {
				// Insert to inventory
				g_hash_table_add(inv,buf);
				
				// Do not reuse memory if it is
				// referenced from hash table
				buf = NULL;
			}
		}

		// Start reading from top
		buf_pos = 0;
		buf_left = sizeof(struct msg);
	}
}
