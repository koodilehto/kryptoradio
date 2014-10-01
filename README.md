<!-- -*- mode: markdown; coding: utf-8 -*- -->

# Kryptoradio – Bitcoin broadcasting tool and more!

The aim of this project is to create a toolset for broadcasting data
to DVB-T and receiving it. It uses Kryptoradio protocol and is
suitable to all kinds of low bandwidth data. The main use case is
Bitcoin broadcasting over digital television.

The Bitcoin serialization tool must be connected to a trusted peer (a
bitcoin node run by yourself) because transactions and blocks are not
verified to keep this tool as simple as possible without compromizing
security. For information about the Bitcoin part, see subdirectory
`bitcoin/`.

The data stream produced by `kryptoradio-encoder` uses PES packet
format that is compatible with DVB encoder hardware.

See our [project page](http://kryptoradio.koodilehto.fi/) for a
longer description.

Email us for more information: <kryptoradio@koodilehto.fi>

## How to just hex dump the traffic

While the receiver tool is still under development there is a hex dumper tool available in the source tree directory [scripts/dump/](https://github.com/koodilehto/kryptoradio/tree/master/scripts/dump). Happy hacking!

## Compiling receiver

The following instructions are for users of Debian and Ubuntu. If you
use another platform, you are on your own. In that case please send me
a pull request of installation instructions for that platform.

Install the environment and libraries first:

	sudo apt-get install cabal-install libghc-aeson-dev libghc-attoparsec-dev libghc-blaze-builder-dev libghc-cmdargs-dev libghc-csv-dev libghc-http-types-dev libghc-stm-dev libghc-text-dev libghc-wai-dev libghc-warp-dev libghc-http-date-dev libghc-random-dev libghc-zlib-dev
	cabal update

Then install Kryptoradio. This will fetch and compile remaining dependencies as well:

	cd receiver
	cabal install

If it compiles, you should have the binary in `~/.cabal/bin/kryptoradio-receiver`.

**Important note for Raspberry Pi users:** If you are building for
Debian Wheezy on `armhf` architechture (like Raspberry Pi) then you have
problems with the old GHC version. To solve that issue, you need a
custom version of aeson library which has all TH functionality
removed. Run the following commands on an **empty directory**:

	git clone -b wheezy_arm_ugly_fix https://github.com/koodilehto/aeson .
	cabal install

Then go back to the original source directory and run `cabal install` again.

## Compiling broadcasting components

You may safely skip this if you want to just receive Kryptoradio. Here
are very brief installation instructions for broadcasting components:

	sudo apt-get install cabal-install libghc-aeson-dev libghc-blaze-builder-dev libghc-cmdargs-dev libghc-http-types-dev libghc-stm-dev libghc-scientific-dev libghc-text-dev libghc-wai-dev libghc-warp-dev libghc-websockets-dev
	cabal update
	cd encoder
	cabal install
	cd ..
	cd data_sources/exchange
	cabal install
	cd ../..

The resulting binaries are `~/.cabal/bin/kryptoradio-exchange` and `~/.cabal/bin/kryptoradio-encoder`.

Even if you don't have the hardware you may run the encoder in "mock" mode. Ask me for help.

## Running

How to run the receiver at 634 MHz:

    ~/.cabal/bin/kryptoradio-receiver 634000000

You can add Cabal binary directory to your PATH to make running it easier. By default it listens to TCP port 3000. See more usage instructions with `kryptoradio-receiver --help`. To find your frequency, you can use this table: http://kryptoradio.koodilehto.fi/media/transmitters

The following entry points are available:

* `/api`
* `/api/waitsync`
* `/api/resources`
* `/api/resource/NAME/raw`
* `/api/resource/NAME/json`
* `/api/resource/NAME/jsoncsv`

To listen to currency exchange information, try this:

    curl -N http://localhost:3000/api/resource/exchange/jsoncsv
	
Sync happens every 60 seconds so you may need wait at most one minute
after starting the receiver before the resource is available. You can
use `/api/waitsync` to wait for the first sync to happen.

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
