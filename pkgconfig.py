# -*- mode: python; coding: utf-8 -*-
import os
from SCons.Script import *

def getConf():
    env = Environment(ENV=os.environ)
    conf = Configure(env, custom_tests = { 'CheckPKGConfig' : CheckPKGConfig,
                                           'CheckPKG' : CheckPKG })

    if not conf.CheckPKGConfig('0.15.0'):
        print 'pkg-config >= 0.15.0 not found.'
        Exit(1)

    return conf

def ensure(conf, name, debian):
    if not conf.CheckPKG(name):
        print (name+' not found.')
        print ("If you have Debian, try: apt-get install "+debian)
        Exit(1)

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
