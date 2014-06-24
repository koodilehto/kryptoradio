/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C ../.." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include <err.h>
#include <stdio.h>
#include <unistd.h>

void deserialize(const int devfd)
{
	char buf[2048];
	const int got = read(devfd,buf,2048);

	if (got == 0) {
		errx(3,"Unexpected end of bitcoin stream");
	} else if (got == -1) {
		err(3,"Error reading bitcoind stream");
	}

	printf("Got %d bytes\n",got);
}
