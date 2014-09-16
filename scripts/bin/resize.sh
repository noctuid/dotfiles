#!/bin/sh
if [ "$1" == "left" ]; then
	bspc window --edge right -40 || bspc window --edge left -40
elif [ "$1" == "down" ]; then
	bspc window --edge up +30 || bspc window --edge down +30
elif [ "$1" == "up" ]; then
	bspc window --edge up -30 || bspc window --edge down -30
elif [ "$1" == "right" ]; then
	bspc window --edge right +40 || bspc window --edge left +40
fi
