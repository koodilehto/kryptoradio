/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
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
#include <errno.h>
#include <glib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <fcntl.h>
#include <unistd.h>
#include "bitcoin.h"

void log_msg(const struct msg *const m)
{
	// Prepare path name. FIXME this has security issue if command
	// string contains slashes. Not a problem if you connect to a
	// trusted peer, though.
	char pathname[4+11+1];
	snprintf(pathname,sizeof(pathname),"log/%s",bitcoin_type_str(m));

	// Create directory. If already exists, ignore error.
	const int ret = mkdir(pathname,0777);
	if (ret != 0 && errno != EEXIST) err(5,"Unable to create directory %s",
					     pathname);

	// Prepare file name
	char filename[4+11+1+64+1];
	snprintf(filename,sizeof(filename),"%s/%s",
		 pathname,hex256(bitcoin_inv_hash(m)));

	// Open file
	int fd = creat(filename,0666);
	if (fd == -1) err(5,"Unable to open %s for writing", filename);

	// Store data to file
	if (m->length > 0 && write(fd,m->payload,m->length) == -1) {
		err(5,"Storing message to log file has failed");
	}

	// Closing log file
	if (close(fd)) err(5,"Closing log file has failed");

	// Report what's done
	printf("Storing %s\n",filename);

	// Make sure debugging strings are printed
	fflush(stdout);
}
