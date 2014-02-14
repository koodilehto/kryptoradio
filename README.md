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

Until I decide to write SConstruct file for this project the flow goes like this:

Install the following packages (moreutils is just needed for `ts`):

    gcc
    libssl-dev
    moreutils

Then compile:

    gcc -Wall -std=gnu99 main.c `pkg-config --libs --cflags openssl`

## Running

Currently there is no fancy command-line parser. Just give IP of a
bitcoin network node you trust and serial port device. Any slow enough
device will do to demonstrate the multitasking. For example:

    mkdir log
    ./a.out 10.0.0.2 /dev/ttyUSB0 | ts '%Y-%m-%d %H:%M:%S %Z' | tee -a log/received.txt

This should do it.

## Supporting

Become a subscriber by sending some bitcoins to 1FvEggFtNSYS9pcBoYB9wDxH9fa1mrNPW5.
