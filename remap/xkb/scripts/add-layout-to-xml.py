#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import print_function
import sys
import xml.etree.ElementTree as ET

def getLayoutXml(layout, description):
    layoutXml = ET.Element('layout')
    configItem = ET.SubElement(layoutXml, 'configItem')
    ET.SubElement(configItem, 'name').text = layout
    ET.SubElement(configItem, 'shortDescription').text = layout
    ET.SubElement(configItem, 'description').text = description
    layoutXml.text = '\n      '
    configItem.text = '\n        '
    for child in configItem[:-1]:
        child.tail = '\n        '
        configItem[-1].tail = '\n      '
        configItem.tail = '\n    '
        layoutXml.tail = '\n  '
    return layoutXml

if len(sys.argv) != 4:
    print('add-layout-to-xml needs exactly 3 argument', file=sys.stderr)
fname = sys.argv[1]
layout = sys.argv[2]
description = sys.argv[3]

tree = ET.parse(fname)
root = tree.getroot()
layoutList = root.find('layoutList')

for layout_xml in layoutList:
    if layout_xml.find('configItem').find('name').text == layout:
        sys.exit(0)

layoutList[-1].tail = '\n    '
layoutList.append(getLayoutXml(layout, description))
root.tail = '\n'
tree.write(fname)
