#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import print_function
import sys
import xml.etree.ElementTree as ET

def getModelXml(mod, description):
    modelXml = ET.Element('model')
    configItem = ET.SubElement(modelXml, 'configItem')
    ET.SubElement(configItem, 'name').text = 'mod_' + mod
    ET.SubElement(configItem, 'description').text = description
    ET.SubElement(configItem, 'vendor').text = 'Generic'
    indent(modelXml, 2)
    modelXml.tail = '\n  '
    return modelXml

def indent(element, level):
    if list(element):
        element.text = '\n' + 2*(level+1)*' '
        for child in element[:-1]:
            indent(child, level+1)
            child.tail = '\n' + 2*(level+1)*' '
        indent(element[-1], level+1)
        element[-1].tail = '\n' + 2*level*' '

if len(sys.argv) != 4:
    print('add-model-to-xml needs exactly 3 arguments', file=sys.stderr)
    sys.exit(1)
fname = sys.argv[1]
mods = sys.argv[2].split('\n') if sys.argv[2] else sys.exit(0)
descriptions = sys.argv[3].split('\n') if sys.argv[3] else sys.exit(0)

tree = ET.parse(fname)
root = tree.getroot()
modelList = root.find('modelList')
for mod, description in zip(mods, descriptions):
    modelList[-1].tail = '\n    '
    modelList.append(getModelXml(mod, description))
root.tail = '\n'
tree.write(fname)
