/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include <err.h>
#include <errno.h>
#include <glib.h>
#include <stdio.h>
#include <string.h>
#include <sys/stat.h> 
#include <sys/types.h> 
#include "bitcoin.h"

void log_msg(const struct msg *const m)
{
	const int payload_len = GUINT32_FROM_LE(m->length_le);
	int hash_end = payload_len;

	if (strcmp(m->command,"inv") == 0) {
		// Inventory messages may be dropped from logs
		return;
	} else if (strcmp(m->command,"block") == 0) {
		// Block hash is calculated only from 6 first fields
		hash_end = 4+32+32+4+4+4;
	}
	
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
	snprintf(filename,sizeof(filename),
		 "log/%s/%s",m->command,hex256(dhash(m->payload,hash_end)));

	// Open file
	FILE *const f = fopen(filename,"wb");
	if (f == NULL) err(5,"Unable to open file for writing %s",filename);
	
	// Store data to file
	if (payload_len > 0 && fwrite(m->payload,payload_len,1,f) != 1) {
		errx(5,"Storing message to log file has failed");
	}

	// Closing log file
	if (fclose(f) != 0) err(5,"Closing log file has failed");

	// Report what's done
	printf("Storing %s\n",filename);

	// Make sure debugging strings are printed
	fflush(stdout);
}
