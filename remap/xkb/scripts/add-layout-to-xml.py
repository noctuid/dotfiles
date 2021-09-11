#!/usr/bin/env python
# -*- coding: utf-8 -*-
from __future__ import print_function
import sys
import xml.etree.ElementTree as ET

def getLayoutXml(layout, description, variants, description_variants):
    layoutXml = ET.Element('layout')
    configItem = ET.SubElement(layoutXml, 'configItem')
    ET.SubElement(configItem, 'name').text = layout
    ET.SubElement(configItem, 'shortDescription').text = layout
    ET.SubElement(configItem, 'description').text = description
    if zip(variants, description_variants):
        variantList = ET.SubElement(layoutXml, 'variantList')
        for variant, description_variant in zip(variants, description_variants):
            variantXml = ET.SubElement(variantList, 'variant')
            variantConfigItem = ET.SubElement(variantXml, 'configItem')
            ET.SubElement(variantConfigItem, 'name').text = variant
            ET.SubElement(variantConfigItem, 'description').text = description + " (" + description_variant + ")"
    indent(layoutXml, 2)
    layoutXml.tail = '\n  '
    return layoutXml

def indent(element, level):
    if list(element):
        element.text = '\n' + 2*(level+1)*' '
        for child in element[:-1]:
            indent(child, level+1)
            child.tail = '\n' + 2*(level+1)*' '
        indent(element[-1], level+1)
        element[-1].tail = '\n' + 2*level*' '

if len(sys.argv) != 6:
    print('add-layout-to-xml needs exactly 5 argument', file=sys.stderr)
    sys.exit(1)
fname = sys.argv[1]
layout = sys.argv[2]
description = sys.argv[3]
variants = sys.argv[4].split('\n') if sys.argv[4] else []
description_variants = sys.argv[5].split('\n') if sys.argv[5] else []

tree = ET.parse(fname)
root = tree.getroot()
layoutList = root.find('layoutList')
layoutList[-1].tail = '\n    '
layoutList.append(getLayoutXml(layout, description, variants, description_variants))
root.tail = '\n'
tree.write(fname)
