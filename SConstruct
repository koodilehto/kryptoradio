# -*- mode: python; coding: utf-8 -*-
import os

env = Environment(ENV=os.environ)

# TODO move pkgconfig and configure boilerplate to another file

def CheckPKGConfig(context, version):
     context.Message( 'Checking for pkg-config... ' )
     ret = context.TryAction('pkg-config --atleast-pkgconfig-version=%s' % version)[0]
     context.Result( ret )
     return ret

def CheckPKG(context, name):
     context.Message( 'Checking for %s... ' % name )
     ret = context.TryAction('pkg-config --exists \'%s\'' % name)[0]
     context.Result( ret )
     return ret

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
