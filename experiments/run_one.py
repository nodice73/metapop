#! /usr/bin/python

# Copyright 2014 Adam Waite
#
# This file is part of metapop.
#
# metapop is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.  
#
# metapop is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with metapop.  If not, see <http://www.gnu.org/licenses/>.

import os, sys

classpath = os.environ.get('CLASSPATH')
classpath = '' if (classpath is None) else classpath + ':'

top = os.path.dirname(os.path.abspath('.'))
prog = sys.argv[1]
args = sys.argv[2]

cmd = ('time java -Xshare:off -Xmx4000m -server '
      '-cp .:{0}{1}/lib/commons-math.jar:{1}/build/classes/framework '
      'org.fhcrc.honeycomb.metapop.experiment.{2} '
      '{3}').format(classpath, top, prog, args)

os.system(cmd)
