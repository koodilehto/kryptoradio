<!-- -*- mode: markdown; coding: utf-8 -*- -->
# How to dump raw Kryptoradio traffic

This does very low-level dump of Kryptoradio traffic in Linux.
Kryptoradio is located in Digita Mux E, pid 8101.

## Supported hardware

If you already have a DVB-T adapter, it's probably supported already. If you don't have one, here are some devices which are known to be compatible:

* http://www.verkkokauppa.com/fi/product/26093/drdhq/A-Link-DTU-DVB-T-digiviritin-tietokoneen-USB-vaylaan
* http://www.dx.com/p/mini-dvb-t-digital-tv-usb-2-0-dongle-with-fm-dab-remote-controller-92096

## How to check signal quality

If you are not certain if you have a antenna TV (DVB-T) or if you are not sure about signal quality, here's a quick check:

1. Connect your TV to the antenna you're planning to use for receiving Kryptoradio
1. Verify that you are **unable** to view channel *Taivas TV7*. If you can see it, you are screwed. And your antenna system is DVB-C.
1. Verify that you can view channel number 11, *Kutonen*.

## Installation

2. Plug DVB-T adapter to your computer
3. Install required Debian/Ubuntu packages and search channels by running `./install`
4. Start `./tune_kryptoradio` and keep it running
5. On another terminal window, start `./watch_kryptoradio`
6. Enjoy!

## QSL cards

In case you'd like to receive a certification of reception, we'll announce QSL card campaign soon! Check web pages for more info when it's there!
