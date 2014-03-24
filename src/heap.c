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

#include <string.h>
#include "heap.h"

// Local functions
static void swap_hashes(void *a, void *b);

void heap_init(struct heap *a)
{
	a->data = NULL;
	a->size = 0;
	a->allocated = 0;
}

heap_item *heap_new_item(struct heap *a)
{
	if (a->size == a->allocated) {
		// Enlarge heap
		a->allocated++;
		a->data = g_renew(heap_item,a->data,a->allocated);
	}

	// Do not mark item as used (size is not incremented)
	return a->data + a->size;
}

void heap_finish_insert(struct heap *a, GCompareDataFunc cmp_func, gpointer cmp_data)
{
	// To ease addressing, we need a 1-based array.
	heap_item *const root = a->data - 1;
	a->size++;
	int i = a->size;

	while (i != 1) {
		// If order is correct, stop.
		gint order = cmp_func(root+(i/2), root+i, cmp_data);
		if (order <= 0) break;

		// Order is not correct, must swap
		swap_hashes(root+(i/2), root+i);
		
		// Going up the tree
		i /= 2;
	}
}

heap_item *heap_pop(struct heap *a, GCompareDataFunc cmp_func, gpointer cmp_data)
{
	// Move to-be-removed item to the end
	a->size--;
	swap_hashes(a->data, a->data+a->size);

	// Start pushing root down
	heap_item *const root = a->data - 1;
	int i=1;

	while (2*i <= a->size) {
		// Find smallest one of the childs
		int smallest_child = 2*i;
		if (2*i == a->size) {
			// Left is smaller because the child on the
			// right is out of bounds.
		} else {
			// If both are there, must use comparator
			gint order = cmp_func(root+2*i, root+2*i+1, cmp_data);
			if (order > 0) smallest_child++;
		}
		
		// Stop if the order is fine.
		gint order = cmp_func(root+i, root+smallest_child, cmp_data);
		if (order <= 0) break;

		// Swap and continue down.
		swap_hashes(root+i, root+smallest_child);
		i = smallest_child;
	}

	// Return data on removed position
	return a->data+a->size;
}

int heap_size(struct heap *a)
{
	return a->size;
}

static void swap_hashes(void *a, void *b)
{
	guint tmp[SHA256_DIGEST_LENGTH];
	memcpy(tmp, a, SHA256_DIGEST_LENGTH);
	memcpy(a, b, SHA256_DIGEST_LENGTH);
	memcpy(b, tmp, SHA256_DIGEST_LENGTH);
}
