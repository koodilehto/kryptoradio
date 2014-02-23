/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
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
	snprintf(pathname,sizeof(pathname),"log/%s",m->command);

	// Create directory. If already exists, ignore error.
	const int ret = mkdir(pathname,0777);
	if (ret != 0 && errno != EEXIST) err(5,"Unable to create directory %s",
					     pathname);

	// Prepare file name
	char filename[4+11+1+64+1];
	snprintf(filename,sizeof(filename),"log/%s/%s",
		 m->command,hex256(bitcoin_inv_hash(m)));

	// Open file
	int fd = creat(filename,0666);
	if (fd == -1) err(5,"Unable to open %s for writing", filename);

	// Store data to file
	const int payload_len = GUINT32_FROM_LE(m->length_le);
	if (payload_len > 0 && write(fd,m->payload,payload_len) == -1) {
		err(5,"Storing message to log file has failed");
	}

	// Closing log file
	if (close(fd)) err(5,"Closing log file has failed");

	// Report what's done
	printf("Storing %s\n",filename);

	// Make sure debugging strings are printed
	fflush(stdout);
}
