# -*- mode: python; coding: utf-8 -*-
import pkgconfig

conf = pkgconfig.getConf()
pkgconfig.ensure(conf,'glib-2.0 >= 2','libglib2.0-dev')
pkgconfig.ensure(conf,'openssl >= 1','libssl-dev')
env = conf.Finish()

env.Append(CCFLAGS = "-O2 -g -Wall -std=gnu99")
env.ParseConfig('pkg-config --cflags --libs openssl')
env.ParseConfig('pkg-config --cflags --libs glib-2.0')
env.ParseConfig('pkg-config --cflags --libs zlib')
env.Program('serializer', [Glob('src/*.c'),Glob('src/serializer/*.c')])
env.Program('deserializer', [Glob('src/*.c'),Glob('src/deserializer/*.c')])
