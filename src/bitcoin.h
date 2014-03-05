/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#ifndef BITCOIN_H_
#define BITCOIN_H_

#include <glib.h>
#include <stdbool.h>

/**
 * Message types. Order also defines priority in transmission. The
 * priority is descending, meaning that BLOCK has priority over TX.
 * UNDEFINED doesn't match anything in Bitcoin Specification and
 * should never be stored (used internally in incoming_node_data()
 * when type is not known yet).
 */
enum msg_type {
	UNDEFINED,
	BLOCK,
	TX,
	ADDR,
	VERSION,
	VERACK,
	INV,
	OTHER,
};

/**
 * Data type for message structure on wire.
 * https://en.bitcoin.it/wiki/Protocol_specification#Message_structure
 */
struct __attribute__ ((__packed__)) msg_wire {
	guint32 magic;
	char command[12];
	guint32 length_le; // Little-endian!
	guint32 checksum;
	guint8 payload[];
};

/**
 * Data type for block which is a contained in payload of msg.
 * https://en.bitcoin.it/wiki/Protocol_specification#block
 */
struct __attribute__ ((__packed__)) block {
	guint32 version;
	guint8 prev_block[32];
	guint8 merkle_root[32];
	guint32 timestamp_le; // Little-endian!
	guint32 bits_le; // Calculated difficulty, little-endian!
	guint32 nonche;
	guint8 txs[]; // Transaction count and list of transactions
};

/**
 * Data type for messages on memory. This format is never used on
 * wire. This is not packed because internal structure is not
 * revealed to the network.
 */
struct msg {
	guint32 length;
	guint32 height;
	bool sent; // Is this sent over the serial link. Maybe this should be a timestamp?
	enum msg_type type; // FIXME this should be guint8 to make it
			    // more compact. This field is included
			    // when calculating signature
	union {
		struct block block;
		guint8 payload[1]; // Workaround for accessing raw data
	};
};

/**
 * Stores pointers to various data structures holding bitcoin
 * inventory items (blocks and transactions) and priority queue for
 * sending messages.
 */
struct bitcoin_storage {
	GHashTable *inv;
	GSequence *send_queue;
};

// TODO function comments

/**
 * Returns contents of var_int located at buf.
 */
guint64 var_int(const guint8 *const buf);

/**
 * Returns number of bytes occupied by var_int located at buf. The
 * result may be 1, 3, 5, or 9.
 */
gint var_int_len(const guint8 *const buf);

/**
 * Returns contents of var_int located at **p and advances pointer at
 * *p to location right after var_int field. */
guint64 get_var_int(const guint8 **p);

/**
 * Calculates checksum for given wire message.
 */
guint32 checksum(const struct msg_wire *const m);

/**
 * Calculates double SHA256 of given data. This is described in
 * Bitcoin Protocol Specification.
 */
guchar *dhash(const guchar *const d, const gulong n, guchar *const md);

/**
 * Returns number of "identifying bytes" in an inventory item. Rest may
 * ignored when calculating the hash.
 */
int bitcoin_hashable_length(const struct msg *const m);

/**
 * Calculates inventory hash from given message using static buffer.
 */
const guchar *const bitcoin_inv_hash(const struct msg *const m);

/**
 * Calculates inventory hash from given message using buffer allocated
 * by the caller. If md is NULL the behaviour is identical to
 * bitcoin_inv_hash().
 */
guchar *bitcoin_inv_hash_buf(const struct msg *const m, guchar *const md);

char *hex256(const guchar *const buf);
bool bitcoin_join(int fd);

/**
 * Returns a new Bitcoin storage which is documented in `struct
 * bitcoin_storage`. Individual elements in should be free'd
 * separately if needed. TODO: Destructor is not yet implemented
 * because this data is never deallocated in current code.
 */
struct bitcoin_storage bitcoin_new_storage();

/**
 * Insert given message to inventory of objects. This function doesn't
 * modify m, but it is not declared as const because glib doesn't have
 * constant pointers to data.
 */
bool bitcoin_inv_insert(struct bitcoin_storage const *st, struct msg *const m);

/**
 * Fetches unsent message with highest sending priority.
 */
struct msg *bitcoin_dequeue(struct bitcoin_storage const *st);

/**
 * Returns message type of given wire message.
 */
enum msg_type bitcoin_find_type(const struct msg_wire *m);

/**
 * Convert msg_type inside msg to string constant. Returns static
 * buffer.
 */
const char* bitcoin_type_str(const struct msg *m);

/**
 * Returns transaction length by peeking inside given buffer.
 */
int bitcoin_tx_len(const guint8 *const buf);

#endif /* BITCOIN_H_ */
