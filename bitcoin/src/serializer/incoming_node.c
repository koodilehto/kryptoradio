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

#include <err.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <openssl/sha.h>
#include "../bitcoin.h"
#include "../log.h"

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
		buf = g_realloc(buf,buf_allocated);
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
			COMPACT->sent = false;
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
		// Free some memory if the buffer is larger than contents
		if (buf_allocated != buf_pos) {
			buf = g_realloc(buf,buf_pos);
		}

		COMPACT->height = UNCONFIRMED;

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
	case BLOCK:
		;
		// Set block height
		const struct msg *parent = g_hash_table_lookup(st->inv,COMPACT->block.prev_block);
		if (parent == NULL) {
			COMPACT->height = 0; // Start orphaned block from 0
		} else {
			COMPACT->height = parent->height+1;
		}

		// Traverse all transactions in the block and allocate
		// memory separately for each transaction.
		guint8 *hash_p = COMPACT->block.txs;
		const guint8 *p = hash_p;
		const guint64 txs = get_var_int(&p);

		for (guint64 tx_i=0; tx_i<txs; tx_i++) {
			// Find out tx length and position and check
			// if tx already exists. Fetch the original hash if it exists
			guchar* key;
			struct msg *tx;
			const int length = bitcoin_tx_len(p);
			const guchar *const ref_hash = dhash(p,length,NULL);
			const bool new_tx = 
				!g_hash_table_lookup_extended(st->inv, ref_hash,
							      (gpointer *)&key,
							      (gpointer *)&tx);

			if (new_tx) {
				// Fill in key data
				key = g_malloc(SHA256_DIGEST_LENGTH);
				memcpy(key,ref_hash,SHA256_DIGEST_LENGTH);

				// Allocate storage for tx
				tx = g_malloc(offsetof(struct msg,payload)+length);
				// Set headers
				tx->length = length;
				tx->sent = false;
				tx->type = TX;
				tx->height = COMPACT->height;

				// Fill with tx data
				memcpy(tx->payload, p, length);

				// Insert
				g_hash_table_insert(st->inv,key,tx);
			} else {
				// Tune existing tx height
				tx->height = COMPACT->height;
			}

			// Debugging
			printf("Block tx %s, %-7s %5d bytes\n",
			       hex256(key), 
			       new_tx ? "new," : tx->sent ? "sent," : "queued,",
			       length);

			// Overwrite transaction data by its hash and
			// advance pointers. Do not touch p after this.
			memcpy(hash_p, key, SHA256_DIGEST_LENGTH);
			hash_p += SHA256_DIGEST_LENGTH;
			p += length;

			// Enqueue transaction if it is not yet sent,
			// even if it is enqueued already.
			if (!tx->sent) {
				bitcoin_enqueue(st,key);
			}
		}

		// Sanity checks
		if (p != COMPACT->payload + COMPACT->length) {
			errx(9,"Block length doesn't match to contents");
		}

		// Free unused memory freed by using hashes
		COMPACT->length = hash_p - COMPACT->payload;
		buf = g_realloc(buf, hash_p - (guint8*)buf);

		// Finally, put the block in send queue
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
