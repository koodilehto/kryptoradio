/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include <err.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "bitcoin.h"
#include "log.h"

// Share memory with compact struct
#define COMPACT ((struct msg *)buf)

// Processes messages having this format:
// https://en.bitcoin.it/wiki/Protocol_specification#Message_structure
void incoming_node_data(const int fd, struct bitcoin_storage *const st)
{
	static struct msg_wire *buf = NULL; // For storing the message payload
	static int buf_allocated = 0;
	static int buf_pos = 0;
	static int buf_left = sizeof(struct msg_wire);
	static enum msg_type buf_type = UNDEFINED;

	// Reallocate buffer only if it is too small.
	if (buf_allocated < buf_pos+buf_left) {
		buf_allocated = buf_pos+buf_left;
		buf = realloc(buf,buf_allocated);
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

	// Header received. To continue reading in next pass, update
	// the message length and possibly compact the header.
	if (buf_type == UNDEFINED && buf_pos == sizeof(struct msg_wire)) {
		// Check message header
		if (GUINT32_FROM_LE(buf->magic) != 0xD9B4BEF9) {
			errx(3,"Magic error. Probably we got out of sync.");
		}

		// Gather message type and length
		buf_type = bitcoin_find_type(buf);
		const guint32 payload_len = GUINT32_FROM_LE(buf->length_le);
		
		// Do header compaction if type is not INV. Doing
		// dangerous in-place rewrite.
		if (buf_type != INV) {
			COMPACT->type = buf_type;
			COMPACT->length = payload_len;
			COMPACT->height = 0;
			// Rewind to compacted payload starting pos
			buf_pos = offsetof(struct msg,payload);
		}
		
		buf_left = payload_len;
	}

	// If not everything is received yet, come back later
	if (buf_left) return;
	
	// All payload received.
	
	// Log message to a flat file (all but invs)
	if (buf_type != INV) log_msg(COMPACT);
	
	// Message valid, parsing it
	switch (buf_type) {
	case INV:
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
		
		break;
	case TX:
	case BLOCK:
		// Free some memory if the buffer is larger than contents
		if (buf_allocated != buf_pos) {
			buf = realloc(buf,buf_pos);
			if (buf == NULL) errx(5,"Memory compaction failed");
		}

		if (bitcoin_inv_insert(st,COMPACT)) {
			// Do not reuse buffer memory because
			// it is stored to the inventory
			buf = NULL;
			buf_allocated = 0;
		} else {
			// If already received, do not do anything
			warnx("Protocol quirk: duplicate %s %s",
			      bitcoin_type_str(COMPACT),
			      hex256(bitcoin_inv_hash(COMPACT)));
		}
		break;
	default:
		// Unknown message type. Do not process it.
		break;
	}
	
	// Start reading from top
	buf_pos = 0;
	buf_left = sizeof(struct msg_wire);
	buf_type = UNDEFINED;
}
