/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C ../.." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include <err.h>
#include <fcntl.h> 
#include <glib.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include "../serial.h"
#include "deserialization.h"

static gchar *serial_dev = NULL;
static gint serial_speed = 0;
static gint node_port = 8333;
static gboolean read_write = false;

static GOptionEntry entries[] =
{
  { "speed", 's', 0, G_OPTION_ARG_INT, &serial_speed, "Serial port baud rate (default: do not set)", "BAUD" },
  { "file", 'f', 0, G_OPTION_ARG_FILENAME, &serial_dev, "Device or file to read for incoming bitstream. (required)", "FILE" },
  { "port", 'p', 0, G_OPTION_ARG_INT, &node_port, "TCP port for listening to incoming bitcoind connections (default: 8333)", "PORT" },
  { "read-write", 'w', 0, G_OPTION_ARG_NONE, &read_write, "Open the file in read-write mode. This is needed if your input is a FIFO.", NULL },
  { NULL }
};

int main(int argc, char *argv[])
{
	GError *error = NULL;
	GOptionContext *context;
	
	context = g_option_context_new("- Bitcoin node which receives transaction and block data from a file or device");
	g_option_context_add_main_entries(context, entries, NULL);
	if (!g_option_context_parse(context, &argc, &argv, &error))
	{
		g_print("option parsing failed: %s\n", error->message);
		exit(1);
	}

	if (serial_dev == NULL) {
		errx(1,"Option --file is mandatory. Try '%s --help'",argv[0]);
	}

	if (argc != 1) {
		errx(1,"Too many arguments on command line. Try '%s --help'",argv[0]);
	}

	// Prepare serial port
	const int access = read_write ? O_RDWR : O_RDONLY;
	const int dev_fd = serial_open_raw(serial_dev,
					   O_NOCTTY | O_NONBLOCK | access,
					   serial_speed);
	if (dev_fd == -1) {
		err(2,"Unable to open serial port %s",serial_dev);
	}

	// Initialize decoder
	struct decoder_state decoder_state;
	deserialize_init(&decoder_state);

	struct pollfd fds[] = {{dev_fd,POLLIN,0}};

	while (true) {
		const int ret = poll(fds,1,-1);
		if (ret < 1) err(5,"Error while polling");

		if (fds[0].revents & POLLIN) {
			deserialize(dev_fd, &decoder_state);
		} else if (fds[0].revents & POLLHUP) {
			errx(2,"The input file is probably a FIFO and the "
			     "feeder process has died. To avoid this, use "
			     "-w option");
		} else {
			warnx("poll() handling is not correct.");
			abort();
		}
	}
}
