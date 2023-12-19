#!/bin/sh

date_formatted=$(date "+%a %F %H:%M")

mute=$(pamixer --get-mute | grep true)
vol=$(pamixer --get-volume)

audio_state="🔊"
if [[ $mute ]]; then
    audio_state="🔇"
fi

audio_formatted="$audio_state $vol%"

echo "$audio_formatted | $date_formatted"
