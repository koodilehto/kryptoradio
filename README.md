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

See our [project page](http://kryptoradio.koodilehto.fi/) for a
longer description.

Email us for more information: <kryptoradio@koodilehto.fi>

## How to just hex dump the traffic

While the receiver tool is still under development there is a hex dumper tool available in the source tree directory [scripts/dump/](https://github.com/koodilehto/kryptoradio/tree/master/scripts/dump). Happy hacking!

## Compiling

To compile Haskell components, install the environment first:

    sudo apt-get install cabal-install libghc-csv-dev libghc-curl-dev libghc-cmdargs-dev libghc-blaze-builder-dev
	cabal update

Not all packages can be found on Debian / Ubuntu or they are too old. In future this will be easier. So installing the remaining packages via Cabal.

Then install Kryptoradio. This will fetch and compile remaining dependencies as well:

    cabal install

## Running

How to run the receiver at 634 MHz:

    ~/.cabal/bin/kryptoradio-receiver 634000000

You can add Cabal binary directory to your PATH to make running it easier. By default it listens to TCP port 3000. See more usage instructions with `kryptoradio-receiver --help`. To find your frequency, you can use this table: http://kryptoradio.koodilehto.fi/media/transmitters

The following entry points are available:

* /api
* /api/waitsync
* /api/resources
* /api/resource/NAME/raw
* /api/resource/NAME/json
* /api/resource/NAME/jsoncsv

To listen to currency exchange information, try this:

    curl -N http://localhost:3000/api/resource/exchange/jsoncsv
	
Sync happens every 60 seconds so you may need wait at most one minute
after starting the receiver before the resource is available. You can
use /api/waitsync to wait for the first sync to happen.

For information about the Bitcoin part, see subdirectory `bitcoin/`.

## License

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU Affero General Public License as
published by the Free Software Foundation, either version 3 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
Affero General Public License for more details.

See LICENSE file for details. The license text is also available at
<http://www.gnu.org/licenses/>.

The intentional cause of licensing under the terms of GNU Affero
General Public License is that if you create a derivate work based on
the source code of this project and start **your own broadcasting
service**, you must **share its source code**. The same restrictions apply
to the receiver. If this is not desired, you are welcome to purchase a
commercial license from Koodilehto Co-op.

**The protocol itself is not covered by any restrictions**. You are
allowed to implement your own transmitter and receiver from scratch
for no licensing costs.

## Supporting and contact information

If you want to donate to the project or co-operate in other ways,
don't hesitate to contact us. Also, if you are planning to integrate
bitcoin-broadcast with your project and you need alternative
licensing, please [contact us](mailto:kryptoradio@koodilehto.fi).

Bitcoin address of this project is [1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5](bitcoin:1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5).

![QR code to 1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5](/misc/bitcoin_address.png)
