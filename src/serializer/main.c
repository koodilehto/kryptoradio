/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include <arpa/inet.h> 
#include <err.h>
#include <fcntl.h>
#include <glib.h>
#include <poll.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "incoming_node.h"
#include "../bitcoin.h"
#include "../serial.h"
#include "serialization.h"

static gchar *serial_dev = NULL;
static gint serial_speed = 0;
static gchar *node_ip = "127.0.0.1";
static gint node_port = 8333;
static gboolean serial_pad = false;

static GOptionEntry entries[] =
{
  { "speed", 's', 0, G_OPTION_ARG_INT, &serial_speed, "Serial port baud rate (default: do not set)", "BAUD" },
  { "file", 'f', 0, G_OPTION_ARG_FILENAME, &serial_dev, "Write bitstream to FILE. Required.", "FILE" },
  { "pad", 'a', 0, G_OPTION_ARG_NONE, &serial_pad, "Send padding when send queue is empty", NULL },
  { "host", 'h', 0, G_OPTION_ARG_STRING, &node_ip, "IP address of bitcoin node to connect (default: 127.0.0.1)", "IP" },
  { "port", 'p', 0, G_OPTION_ARG_INT, &node_port, "TCP port of bitcoin node to connect (default: 8333)", "PORT" },
  { NULL }
};

int main(int argc, char *argv[])
{
	GError *error = NULL;
	GOptionContext *context;
	
	context = g_option_context_new("- Serializes bitcoin blocks and transactions");
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
	int dev_fd = serial_open_raw(serial_dev, O_NOCTTY|O_WRONLY|O_NONBLOCK,
				     serial_speed);
	if (dev_fd == -1) {
		err(2,"Unable to open serial port %s",serial_dev);
	}

	// Prepare socket

	int node_fd;
	struct sockaddr_in serv_addr;

	if((node_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		err(2,"Could not create socket");
	}

	memset(&serv_addr, '0', sizeof(serv_addr)); 

	serv_addr.sin_family = AF_INET;
	serv_addr.sin_port = htons(node_port); 

	if(inet_pton(AF_INET, node_ip, &serv_addr.sin_addr) <= 0) {
		errx(1,"IP address conversion failed");
	} 

	if(connect(node_fd, (struct sockaddr *)&serv_addr,
		   sizeof(serv_addr)) < 0)
	{
		err(2,"Connect failed");
	}

	// Send greeting
	if (bitcoin_join(node_fd)) {
		err(2,"Sending of welcome message has failed");
	}

	// Prepare local data for incoming_node_data
	struct bitcoin_storage st = bitcoin_new_storage();

	printf("Connected.\n");

	struct pollfd fds[] = {{node_fd,POLLIN,0},{dev_fd,POLLOUT,0}};
	bool serial_enabled = true;

	// Process messages forever
	while (true) {
		const int ret = poll(fds,serial_enabled ? 2 : 1,-1);
		if (ret < 1) err(5,"Error while polling");

		// Always serve slow serial first
		if (serial_enabled && (fds[1].revents & POLLOUT)) {
			serial_enabled = serialize(dev_fd,&st,serial_pad);
		}
		if (fds[0].revents & POLLIN) {
			incoming_node_data(node_fd,&st);
			serial_enabled = true;
		}
	}
}
