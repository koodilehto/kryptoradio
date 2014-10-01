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

## How to install and use Kryptoradio receiver

See [receiver/](receiver).

## How to compile broadcasting components

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
