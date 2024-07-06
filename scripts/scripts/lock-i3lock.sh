#!/bin/bash

SCROT_PATH=/tmp/lock-scrot.png

LOCK_PIXEL_SMALLNESS=${LOCK_PIXEL_SMALLNESS:-5}

shrink_scale="${LOCK_PIXEL_SMALLNESS}%"
magnify_scale="$(python -c "print(10000 / $LOCK_PIXEL_SMALLNESS)")%"

scrot -o $SCROT_PATH

# convert $SCROT_PATH -blur 0x9 $SCROT_PATH

convert -scale $shrink_scale -scale $magnify_scale $SCROT_PATH $SCROT_PATH
convert $SCROT_PATH ~/scripts/res/lock.png -gravity center -composite -matte $SCROT_PATH

i3lock -i $SCROT_PATH
