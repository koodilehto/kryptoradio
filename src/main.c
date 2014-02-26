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

#define SERIAL_ESC 0xC0

// Prototypes
void serial(const int devfd, struct bitcoin_storage *const st);

int main(int argc, char *argv[])
{
	if(argc != 3) {
		errx(1,"Usage: %s <ip of server> <serial_port>",argv[0]);
	} 

	// Prepare serial port
	
	// TODO baud rate

	int dev_fd = open(argv[2],O_WRONLY|O_NOCTTY|O_NONBLOCK);
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
	static const struct msg *m = NULL;
	static int pos = -1;

	gint size = g_sequence_get_length(st->send_queue);

	if (m == NULL && size > 0) {
		m = bitcoin_dequeue(st);
		pos = -1;
	}

	if (m == NULL) {
		// Send empty stuff and go back to waiting loop
		const char buf[1024];
		memset(&buf,SERIAL_ESC,sizeof(buf));
		const int ret = write(devfd,buf,sizeof(buf));
		if (ret < 1) err(4,"Unable to write to serial port");
		printf("Sending %d bytes of padding\n",ret);
		return;
	}

	printf("Sending %s %s, pos %d/%d, queue size %d\n",
	       bitcoin_type_str(m),
	       hex256(bitcoin_inv_hash(m)),
	       pos,
	       m->length,
	       size);

	if (pos == -1) {
		const int ret = write(devfd,&m->type,1); // FIXME byte ordering issue
		if (ret < 1) err(4,"Unable to write to serial port");
		pos++;
		return;
	}

	int ret = write(devfd,m->payload+pos,m->length-pos);
	if (ret < 1) err(4,"Unable to write to serial port");
	pos += ret;
	if (pos == m->length) {
		m = NULL;
	}
}
