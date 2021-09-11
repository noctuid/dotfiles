#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import print_function
import sys
import xml.etree.ElementTree as ET

if len(sys.argv) != 3:
    print('remove-models-from-xml needs exactly 2 argument', file=sys.stderr)
    sys.exit(1)
fname = sys.argv[1]
mods = sys.argv[2].split('\n') if sys.argv[2] else sys.exit(0)
modsWithPrefix = ['mod_' + mod for mod in mods]

tree = ET.parse(fname)
root = tree.getroot()
modelList = root.find('modelList')
modelList[:] = [modelXml for modelXml in modelList if modelXml.find('configItem').find('name').text not in modsWithPrefix]
modelList[-1].tail = '\n  '
root.tail = '\n'
tree.write(fname)
