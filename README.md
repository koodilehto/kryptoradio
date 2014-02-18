<!-- -*- mode: markdown; coding: utf-8 -*- -->

# Bitcoin broadcastion tool

The aim of this project is to create a tool which serializes bitcoin
peer-to-peer network flow and generates constant-bitrate stream for
broadcasting. This software requires a trusted peer because
transactions and blocks are not verified.

Current version just connects to given bitcoin node, is capable of
requesting transaction contents, and logs all network messages to a
file. In addition, a stub of serial connectivity has been added.

## Compiling

The project uses SCons for building and requires OpenSSL and GLib.
Before compiling, install the following packages (moreutils is just
needed for `ts`):

	gcc
	libssl-dev
	libglib2.0-dev
	scons
	moreutils

Then compile:

	scons

## Running

Currently there is no fancy command-line parser. Just give IP of a
bitcoin network node you trust and serial port device. Any slow enough
device will do to demonstrate the multitasking. For example:

	mkdir log
	./serializer 10.0.0.2 /dev/ttyUSB0 2>&1 | ts '%Y-%m-%d %H:%M:%S %Z' | tee -a log/received.txt

This should do it.

## Supporting

Become a subscriber by sending some bitcoins to 1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5.
