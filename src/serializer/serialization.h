/* -*- mode: c; c-file-style: "linux"; compile-command: "scons -C .." -*-
 *  vi: set shiftwidth=8 tabstop=8 noexpandtab:
 */

#include "../bitcoin.h"

/**
 * Dequeues items from send queue and serialize them to a file
 * descriptor. If serial_pad is true, then add write padding to devfd
 * until it there is real data available. If serial_pad is false, then
 * no padding is used and this function returns false if there is
 * nothing to send. Otherwise true is returned.
 */
bool serialize(const int devfd, struct bitcoin_storage *const st, bool serial_pad);
