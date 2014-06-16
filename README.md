<!-- -*- mode: markdown; coding: utf-8 -*- -->

# Bitcoin broadcasting tool

The aim of this project is to create a tool which serializes bitcoin
peer-to-peer network flow and generates constant-bitrate stream for
broadcasting. The serialization tool must be connected to a trusted
peer (a bitcoin node run by yourself) because transactions and blocks
are not verified to keep this tool as simple as possible without
compromizing security.

The resulting stream may be transmitted over any unidirectional
channel like DVB-T and FM subcarrier.

Current version connects to given bitcoin node, is capable of
requesting transaction and block contents, serializing traffic, and
logging all network messages to a log directory.

See our [project page](http://bitcoinradio.koodilehto.fi/) for a
longer description.

Email me for more information: <joel.lehtonen+broadcast@iki.fi>

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

The default baud rate is 9600 bps, which is sufficient for current
volume of bitcoin traffic. This application has no limitations on
bitrates (has been tested with 1Mbps) and the only limit is the serial
hardware you are using.

## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or (at
your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

See LICENSE file for details. The license text is also available at
<http://www.gnu.org/licenses/>.

## Supporting and contact information

If you want to donate to the project or co-operate in other ways,
don't hesitate to contact us. Also, if you are planning to integrate
bitcoin-broadcast with your project and you need alternative
licensing, please [contact us](mailto:joel.lehtonen+broadcast@iki.fi).

Bitcoin address of this project is [1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5](bitcoin:1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5).

![QR code to 1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5](/misc/bitcoin_address.png)
