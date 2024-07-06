#!/bin/bash

LOCK_MINUTES=${LOCK_MINUTES:-15}
SLEEP_MINUTES=${SLEEP_MINUTES:-60}
SUSPEND_MINUTES=${SUSPEND_MINUTES:-180}

lock_seconds=$(python -c "print(int(60 * $LOCK_MINUTES))")
sleep_seconds=$(python -c "print(int(60 * $SLEEP_MINUTES))")
suspend_seconds=$(python -c "print(int(60 * $SUSPEND_MINUTES))")

echo "lock_seconds   : $lock_seconds"
echo "sleep_seconds  : $sleep_seconds"
echo "suspend_seconds: $suspend_seconds"

swayidle -w timeout $lock_seconds "$HOME/scripts/lock-swaylock.sh" \
	    timeout $sleep_seconds "hyprctl dispatch dpms off" \
	    resume "hyprctl dispatch dpms on" \
	    timeout $suspend_seconds 'systemctl suspend' \
	    before-sleep "$HOME/scripts/lock-swaylock.sh" &
