<!-- -*- mode: markdown; coding: utf-8 -*- -->
# How to dump raw Kryptoradio traffic

This does very low-level dump of Kryptoradio traffic in Linux.
Kryptoradio is located in Digita Mux E, pid 8101.

## Supported hardware

If you already have a DVB-T adapter, it's probably supported already. If you don't have one, here are some devices which are known to be compatible:

* http://www.verkkokauppa.com/fi/product/26093/drdhq/A-Link-DTU-DVB-T-digiviritin-tietokoneen-USB-vaylaan
* http://www.dx.com/p/mini-dvb-t-digital-tv-usb-2-0-dongle-with-fm-dab-remote-controller-92096

## Installation

1. Optionally check with your TV that you can view channel *Kutonen* (channel number 11 in your TV)
2. Plug DVB-T adapter to your computer
3. Install required Debian/Ubuntu packages and tune channels by running `./install`
4. Start `./tune_kryptoradio` and keep it running
5. On another terminal window, start `./watch_kryptoradio`
6. Enjoy!

## QSL cards

In case you'd like to receive a certification of reception, we'll announce QSL card campaign soon! Check web pages for more info when it's there!
