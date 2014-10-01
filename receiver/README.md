<!-- -*- mode: markdown; coding: utf-8 -*- -->

# Kryptoradio receiver

The application *kryptoradio-receiver* tunes to given frequency and PID on DVB-T and
offers an HTTP interface to that data.

## Compiling

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
