#!/bin/bash

export SWWW_TRANSITION_TYPE=random
export WALLDIR=$HOME/Pictures/cyclewall

INTERVAL=300

while true; do
    img=$(ls $WALLDIR | grep -P ".(jpg)|(png)" | sort -R | head -n1)
    imgpath="$WALLDIR/$img"
    echo "Switching wallpaper to $imgpath"
    swww img "$imgpath"
    sleep $INTERVAL
done
