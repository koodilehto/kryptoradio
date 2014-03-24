/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 *
 *  Copyright 2014 Joel Lehtonen 
 *  
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  (at your option) any later version.
 *  
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *  
 *  You should have received a copy of the GNU General Public License
 *  along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

/*
 * Minimum heap which doesn't break if comparator is a bit
 * unstable. In our send queue items may gain higher priority if they
 * are sent or included in the block. This heap supports only SHA256
 * hashes and uses Glib comparators. The implementation is minimal and
 * has some deficiencies: There is no destructor and storage is never
 * freed if number of elements in the heap decreases. It reuses
 * storage so no real memory leak occurs.
 */

#ifndef HEAP_H_
#define HEAP_H_

#include <openssl/sha.h>
#include <glib.h>

/**
 * Heap item is an SHA256 hash
 */
typedef guint8 heap_item[SHA256_DIGEST_LENGTH];

/**
 * Heap contains pointer to heap array and size information.
 */
struct heap {
	heap_item *data;
	int size;
	int allocated;
};

/**
 * Initializes the heap. The struct must be pre-allocated.
 */
void heap_init(struct heap *a);

/**
 * Gets storage for the item. This must be called prior
 * insertion. After calling this you need to fill in the
 * hash. Insertion is finished using heap_finish_insert(). If you
 * decide not to insert, just don't call heap_finish_insert(). You
 * don't need to undo heap_new_item() because it may be called
 * multiple times.
 */
heap_item *heap_new_item(struct heap *a);

/**
 * Finish insertion. Severe damage is done if you call this function
 * without calling heap_new_item() first. This call expects there is a
 * new item in the buffer returned by heap_new_item() and it places it
 * to the correct position in the heap.
 */
void heap_finish_insert(struct heap *a, GCompareDataFunc cmp_func, gpointer cmp_data);

/**
 * Removes smallest item from the heap.
 */
heap_item *heap_pop(struct heap *a, GCompareDataFunc cmp_func, gpointer cmp_data);

/**
 * Returns the number of items in the heap.
 */
int heap_size(struct heap *a);

#endif /* HEAP_H_ */
