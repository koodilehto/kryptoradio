# -*- mode: python; coding: utf-8 -*-
import os
from pkgconfig import *

env = Environment(ENV=os.environ)
conf = Configure(env, custom_tests = { 'CheckPKGConfig' : CheckPKGConfig,
                                       'CheckPKG' : CheckPKG })

if not conf.CheckPKGConfig('0.15.0'):
     print 'pkg-config >= 0.15.0 not found.'
     Exit(1)

if not conf.CheckPKG('glib-2.0 >= 2'):
     print 'glib >= 2 not found.'
     Exit(1)

if not conf.CheckPKG('openssl >= 1'):
     print 'openssl >= 1 not found.'
     Exit(1)

env = conf.Finish()

env.Append(CCFLAGS = "-O2 -g -Wall -std=gnu99")
env.ParseConfig('pkg-config --cflags --libs openssl')
env.ParseConfig('pkg-config --cflags --libs glib-2.0')
env.Program('serializer', Glob('src/*.c'))
