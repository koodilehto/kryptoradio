# -*- mode: python; coding: utf-8 -*-
import pkgconfig

conf = pkgconfig.getConf()
pkgconfig.ensure(conf,'glib-2.0 >= 2','libglib2.0-dev')
pkgconfig.ensure(conf,'openssl >= 1','libssl-dev')
env = conf.Finish()

env.Append(CCFLAGS = "-O2 -g -Wall -std=gnu99")
env.ParseConfig('pkg-config --cflags --libs openssl')
env.ParseConfig('pkg-config --cflags --libs glib-2.0')
env.Program('serializer', Glob('src/*.c'))
