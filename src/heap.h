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
 * Minimum heap
 * 
 * This heap is somewhat modelled after Glib sequences and it uses
 * Glib comparators and data types. The implementation is minimal and
 * has some deficiencies: There is no destructor and storage is never
 * freed if number of elements in the heap decreases. Anyway, it
 * reuses storage so no real memory leak occurs.
 *
 * Special care is taken to support cases where comparator is a bit
 * unstable. In our send queue items may gain higher priority if they
 * are sent or included in the block so this is safer than using Glib
 * sequences which use balanced trees. In these cases just insert the
 * item again if its priority changes and keep a separate book of
 * popped (=sent) items.
 */

#ifndef HEAP_H_
#define HEAP_H_

#include <glib.h>

/**
 * Heap contains pointer to heap array and size information.
 */
struct heap {
	gpointer *data;
	int size;
	int allocated;
};

/**
 * Initializes the heap. The struct must be pre-allocated.
 */
void heap_init(struct heap *a);

/**
 * Insert item to the heap. The use of comparator is consistent with
 * Glib sequences. Insertion complexity is O(log n).
 */
void heap_insert(struct heap *a, gpointer data, GCompareDataFunc cmp_func, gpointer cmp_data);

/**
 * Removes smallest item from the heap. The pointer is not freed.
 */
gpointer heap_pop(struct heap *a, GCompareDataFunc cmp_func, gpointer cmp_data);

/**
 * Returns the number of items in the heap.
 */
int heap_size(struct heap *a);

#endif /* HEAP_H_ */
