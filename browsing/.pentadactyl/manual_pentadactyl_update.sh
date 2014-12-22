#!/bin/sh
# http://5digits.org/coding
cd ~/
name="dactyl_$(date +'%d.%y')"
hg clone http://dactyl.googlecode.com/hg/ $name
cd $name
make -C pentadactyl install
