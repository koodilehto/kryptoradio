/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include <arpa/inet.h> 
#include <err.h>
#include <fcntl.h>
#include <glib.h>
#include <poll.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include "incoming_node.h"
#include "bitcoin.h"

// Prototypes
void serial(const int fd);

int main(int argc, char *argv[])
{
	if(argc != 3) {
		errx(1,"Usage: %s <ip of server> <serial_port>",argv[0]);
	} 

	// Prepare serial port
	
	// TODO baud rate

	int dev_fd = open(argv[2],O_WRONLY|O_NOCTTY);
	if (dev_fd == -1) {
		err(2,"Unable to open serial port %s",argv[2]);
	}

	// Prepare socket

	int node_fd;
	struct sockaddr_in serv_addr;

	if((node_fd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		err(2,"Could not create socket");
	}

	memset(&serv_addr, '0', sizeof(serv_addr)); 

	serv_addr.sin_family = AF_INET;
	serv_addr.sin_port = htons(8333); 

	if(inet_pton(AF_INET, argv[1], &serv_addr.sin_addr) <= 0) {
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

	printf("Connected.\n");

	struct pollfd fds[] = {{dev_fd,POLLOUT,0},{node_fd,POLLIN,0}};

	// Process messages forever
	while (true) {
		const int ret = poll(fds,2,-1);
		if (ret < 1) err(5,"Error while polling");

		// Always serve slow serial first
		if (fds[0].revents & POLLOUT) serial(dev_fd);
		if (fds[1].revents & POLLIN) incoming_node_data(node_fd);
	}
}

void serial(const int devfd) {
	// Just dummy writer for generating much traffic to serial
	static guint8 i=0;
	char instanssi[43];
	snprintf(instanssi,sizeof(instanssi),
		 "Kissa kissa kissa kissa... Instanssi! %3hhu\n",
		 i++);

	// Write the output. Do not care if some output is ignored.
	const int ret = write(devfd,instanssi,sizeof(instanssi)-1);
	if (ret < 1) err(4,"Unable to write to serial port");
	printf("sent %d bytes to serial port\n",ret);
}
