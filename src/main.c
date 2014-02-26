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
void serial(const int devfd, struct bitcoin_storage *const st);

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

	// Prepare local data for incoming_node_data
	struct bitcoin_storage st = bitcoin_new_storage();

	printf("Connected.\n");

	struct pollfd fds[] = {{dev_fd,POLLOUT,0},{node_fd,POLLIN,0}};

	// Process messages forever
	while (true) {
		const int ret = poll(fds,2,-1);
		if (ret < 1) err(5,"Error while polling");

		// Always serve slow serial first
		if (fds[0].revents & POLLOUT) serial(dev_fd,&st);
		if (fds[1].revents & POLLIN) incoming_node_data(node_fd,&st);
	}
}

void serial(const int devfd, struct bitcoin_storage *const st)
{
	gint size = g_sequence_get_length(st->send_queue);

	if (size==0) {
		const char buf[] = "empty ";
		const int ret = write(devfd,buf,sizeof(buf)-1);
		if (ret < 1) err(4,"Unable to write to serial port");
	} else {
		const struct msg *m = bitcoin_dequeue(st);

		printf("Sending %s %s, queue size is %d\n",
		       bitcoin_type_str(m),
		       hex256(bitcoin_inv_hash(m)),
		       size);

		// FIXME no two writes inside single call!
		const int ret1 = write(devfd,&m->type,1); // FIXME byte ordering issue
		if (ret1 < 1) err(4,"Unable to write to serial port");
		const int ret2 = write(devfd,m->payload,m->length);
		if (ret2 < 1) err(4,"Unable to write to serial port");
	}
}
