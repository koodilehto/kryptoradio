/* -*- mode: c; c-file-style: "linux" -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include <sys/socket.h>
#include <sys/types.h>
#include <netinet/in.h>
#include <netdb.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
//#include <errno.h>
#include <arpa/inet.h> 
#include <err.h>
#include <stdbool.h>

uint8_t join_message[] = {
	0xF9, 0xBE, 0xB4, 0xD9, 0x76, 0x65, 0x72, 0x73, 0x69, 0x6F,
	0x6E, 0x00, 0x00, 0x00, 0x00, 0x00, 0x64, 0x00, 0x00, 0x00,
	0x35, 0x8D, 0x49, 0x32, 0x62, 0xEA, 0x00, 0x00, 0x01, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x11, 0xB2, 0xD0, 0x50,
	0x00, 0x00, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0xFF, 0xFF, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xFF, 0xFF,
	0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x3B, 0x2E, 0xB3, 0x5D,
	0x8C, 0xE6, 0x17, 0x65, 0x0F, 0x2F, 0x53, 0x61, 0x74, 0x6F,
	0x73, 0x68, 0x69, 0x3A, 0x30, 0x2E, 0x37, 0x2E, 0x32, 0x2F,
	0xC0, 0x3E, 0x03, 0x00
};

const int join_message_len = sizeof(join_message);

struct __attribute__ ((__packed__)) msg_header {
	uint32_t magic;
	char command[12];
	uint32_t length;
	uint32_t checksum;
};

struct __attribute__ ((__packed__)) msg_inv_vect {
	uint32_t type;
	char hash[32];
};

uint64_t var_int(uint8_t *buf);
int var_int_len(uint8_t *buf);

int main(int argc, char *argv[])
{
	int sockfd;
	struct sockaddr_in serv_addr; 

	if(argc != 2) {
		errx(1,"Usage: %s <ip of server>",argv[0]);
	} 

	if((sockfd = socket(AF_INET, SOCK_STREAM, 0)) < 0) {
		err(2,"Could not create socket");
	}

	memset(&serv_addr, '0', sizeof(serv_addr)); 

	serv_addr.sin_family = AF_INET;
	serv_addr.sin_port = htons(8333); 

	if(inet_pton(AF_INET, argv[1], &serv_addr.sin_addr) <= 0) {
		errx(1,"IP address conversion failed");
	} 

	if( connect(sockfd, (struct sockaddr *)&serv_addr, sizeof(serv_addr)) < 0) {
		err(2,"Connect Failed");
	} 

	// Making FILE from socket fd
	FILE *bitcoind = fdopen(sockfd,"r+");
	if (bitcoind == NULL) err(2,"Buffered reader has failed");

	// Send greeting
	if (fwrite(join_message,join_message_len,1,bitcoind) != 1) {
		errx(2,"Sending of welcome message has failed");
	}

	printf("Connected.\n");

	// Message receiver loop. Processes messages having this format:
	// https://en.bitcoin.it/wiki/Protocol_specification#Message_structure
	while (true) {
		struct msg_header header;
		uint8_t payload[1024];

		// Take header
		if (fread(&header,sizeof(header),1,bitcoind) != 1) {
			errx(3,"Receiving data from bitcoind has failed");
		}

		// FIXME One should change byte ordering on big-endian machines!

		printf("%s, %d tavua.\n",header.command,header.length);

		if (header.length > sizeof(payload)) {
			errx(4,"Too small receive buffer, exiting.");
		}

		if (header.length != 0 && fread(payload,header.length,1,bitcoind) != 1) {
			errx(3,"Receiving data from bitcoind has failed");
		}

		// Parsing it
		if (strcmp(header.command,"inv") == 0) {
			// Match structure to data
			uint64_t invs = var_int(payload);
			struct msg_inv_vect *inv =
				(struct msg_inv_vect*)(payload+var_int_len(payload));
			
			// Pretty-print transaction hash
			for (uint64_t i = 0; i<invs; i++) {
				printf("inv #%d: ",i);
				for (int j=31; j>=0; j--) {
					printf("%02hhx",inv[i].hash[j]);
				}
				printf("\n");
			}
		}
	}

	// Never reached
}

uint64_t var_int(uint8_t *buf)
{
	if (*buf == 0xfd) return *(uint16_t*)(buf+1);
	if (*buf == 0xfe) return *(uint32_t*)(buf+1);
	if (*buf == 0xff) return *(uint64_t*)(buf+1);
	return *buf;
}

int var_int_len(uint8_t *buf)
{
	if (*buf == 0xfd) return 3;
	if (*buf == 0xfe) return 5;
	if (*buf == 0xff) return 9;
	return 1;
}
