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

#include <glib.h>
#include <stdbool.h>
#include <zlib.h>

struct decoder_state {
	guint8 *buf;
	int size;
	z_stream zlib;
	bool has_sync;
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
