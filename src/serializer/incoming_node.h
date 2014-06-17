/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include "../bitcoin.h"

/**
 * Processes data coming from file descriptor which is assumed to be a
 * socket. This function must not block.
 */
void incoming_node_data(const int fd, struct bitcoin_storage *const st);
