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

/* Set serial parameters using non-standard baud rate hack:
 * http://stackoverflow.com/questions/3192478/specifying-non-standard-baud-rate-for-ftdi-virtual-serial-port-under-linux
 * and
 * http://stackoverflow.com/questions/4968529/how-to-set-baud-rate-to-307200-on-linux */

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <termios.h>
#include <sys/ioctl.h>
#include <linux/serial.h>
#include <errno.h>
#include <unistd.h>

int serial_open_raw(const char *dev, int flags, int speed)
{
	// Open device
	int fd = open(dev, flags);
	if (fd == -1) goto not_open;

	// Find current configuration
	struct serial_struct serial;
	if(ioctl(fd, TIOCGSERIAL, &serial) == -1) goto fail;
	serial.flags = (serial.flags & ~ASYNC_SPD_MASK) | ASYNC_SPD_CUST;
	serial.custom_divisor = (serial.baud_base + (speed / 2)) / speed;
	
	// Check that the serial timing error is no more than 2%
	int real_speed = serial.baud_base / serial.custom_divisor;
	if (real_speed < speed * 98 / 100 || real_speed > speed * 102 / 100) {
		errno = ENOTSUP;
		goto fail;
	}

	// Activate
	if(ioctl(fd, TIOCSSERIAL, &serial) == -1) goto fail;

	// Start with raw values
	struct termios term;
	term.c_cflag = B38400 | CS8 | CLOCAL | CREAD; 
	cfmakeraw(&term);
	if (tcsetattr(fd,TCSANOW,&term) == -1) goto fail;

	return fd;
fail:
	close(fd);
not_open:
	return -1;
}
