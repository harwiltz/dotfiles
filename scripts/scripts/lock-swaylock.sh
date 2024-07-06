#!/bin/bash

# SCROT_PATH=/tmp/lock-grim.png

SCROT_DIR=/tmp
SCROT_NAME=lock-grim.png

LOCK_PIXEL_SMALLNESS=${LOCK_PIXEL_SMALLNESS:-5}

VERIF_COLOR=ffffff66
WRONG_COLOR=ff2222aa
CLEAR_COLOR=880066aa

shrink_scale="${LOCK_PIXEL_SMALLNESS}%"
magnify_scale="$(python -c "print(10000 / $LOCK_PIXEL_SMALLNESS)")%"

monitors=$(hyprctl -j monitors | jq -r 'map(.name)' | jq -r '.[]')

swaylock_img_args=""

for monitor in $monitors; do
    scrot_path="$SCROT_DIR/${monitor}_${SCROT_NAME}"
    grim -o $monitor $scrot_path
    convert -scale $shrink_scale -scale $magnify_scale $scrot_path $scrot_path
    convert $scrot_path ~/scripts/res/lock.png -gravity center -composite -matte $scrot_path
    swaylock_img_args="-i ${monitor}:${scrot_path} ${swaylock_img_args}"
done

swaylock $swaylock_img_args \
	 --indicator-radius 200 \
	 --inside-ver-color $VERIF_COLOR \
	 --ring-ver-color $VERIF_COLOR \
	 --line-ver-color ffffff00 \
	 --inside-wrong-color 00000066 \
	 --ring-wrong-color $WRONG_COLOR \
	 --line-wrong-color ffffff00 \
	 --text-wrong-color ffffff \
	 --inside-clear-color $CLEAR_COLOR \
	 --ring-clear-color $CLEAR_COLOR \
	 --line-clear-color ffffff00 \
	 --text-clear-color ffffff \
	 --font Inconsolata &

