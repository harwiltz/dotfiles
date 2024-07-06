#!/bin/bash

MUSIC_DIR=~/Music

album_dirs=$(find $MUSIC_DIR -maxdepth 2 -mindepth 2 -type d -printf '%P\n')
shuffled_album_dirs=$(echo "$album_dirs" | sort -R --random-source=/dev/urandom)

while IFS= read -r album_dir; do
	artist=$(echo "$album_dir" | cut -f1 -d '/')
	album=$(echo "$album_dir" | cut -f2 -d '/')
	echo "$artist: $album"
	read -rp "> Wanna listen? (y/n/q) " answer </dev/tty
	if [[ $answer == [Qq]* ]]; then exit 0; fi
	if [[ $answer == [Yy]* ]]; then
		# mpc clear
		echo "$MUSIC_DIR/$album_dir"
		mpc add "$album_dir"
		echo ">> Queued."
	else
		echo ">> On to the next one..."
	fi
done <<<"$shuffled_album_dirs"
