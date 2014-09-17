<!-- -*- mode: markdown; coding: utf-8 -*- -->

# Bitcoin serialization tool

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
	./serializer -h 10.0.0.2 -f /dev/ttyUSB0 2>&1 | ts '%Y-%m-%d %H:%M:%S %Z' | tee -a log/received.txt

The default baud rate is 9600 bps, which is sufficient for current
volume of bitcoin traffic. This application has no limitations on
bitrates (has been tested with 1Mbps) and the only limit is the serial
hardware you are using.
