#!/bin/sh

KEYS="/home/harwiltz/.dzen/PianoKeysIcon.xbm"
TREBLE="/home/harwiltz/.dzen/Treble.xbm"
PAUSED="/home/harwiltz/.dzen/Paused.xbm"
PAUSED2="/home/harwiltz/.dzen/Paused2.xbm"
STOPPED="/home/harwiltz/.dzen/Stopped.xbm"
HW="/home/harwiltz/.dzen/HW.xbm"
SPEAKER="/home/harwiltz/.dzen/Speaker.xbm"
SPEAKER2="/home/harwiltz/.dzen/Speaker2.xbm"
MUTED="/home/harwiltz/.dzen/Muted.xbm"
MUTED2="/home/harwiltz/.dzen/Muted2.xbm"
MUTED3="/home/harwiltz/.dzen/Muted3.xbm"
WORKSPACE1="/home/harwiltz/.dzen/Workspace1.xbm"
WORKSPACE2="/home/harwiltz/.dzen/Workspace2.xbm"
WORKSPACE2FREE="/home/harwiltz/.dzen/Workspace2Free.xbm"
UNDERLINE="/home/harwiltz/.dzen/Underline.xbm"
ARCHLOGO="/home/harwiltz/.dzen/ArchLogo.xbm"
HWLOGO="/home/harwiltz/.dzen/HW.xbm"
TWO=2

SCREENWIDTH=1920.0
WREL=0.9
WIDTH=$(echo "${SCREENWIDTH}*${WREL}" |bc)
X=$(echo "${SCREENWIDTH}/2 -${WIDTH}/2" |bc)

WIFI_INTERFACE=wlp0s20f3

clock() {
	cal=$(date +'%l:%M:%S %p')
	clo=$(date +'%a, %b %_d')
	location=310*16

	echo -ne "^p(_RIGHT)^p(-$location)^fg(#aaaaaa)$cal | $clo^fg()"
}

workspaces() {
	color_focus=#aaaa00
	color_normal=#444444
	color_busy=#aaaaaa
	names=(" I  " " II " "III " " IV " " V  " " VI " " VII" "VIII" " IX " "  X ")
	line=""
	i=0

	busy=0

	for workspace in $(bspc query -D); do
		busy=0
		icon=$WORKSPACE2FREE
		if [[ $(bspc query -N -d ${workspace}.occupied) ]]; then
			busy=1
			icon=$WORKSPACE2
		fi

		if [[ $workspace = $(bspc query -d -D) ]]; then
			line="${line} ^fg($color_focus)^i($icon)"
		else
			if [[ $busy = 1 ]]; then
				line="${line} ^fg($color_busy)^i($WORKSPACE2)"
			else
				line="${line} ^fg($color_normal)^i($WORKSPACE2FREE)"
			fi
		fi

		i=$(echo "${i} + 1")
	done

	echo -ne "^p(_LEFT)^p(0)${line}^fg()"
}

wifi() {
	color_con=#66bb66
	color_off=#333333
	color_side=#aaaaaa
	location=510*16
	size=10
	total=70
  connection=$(cat /proc/net/wireless| sed -e "s/$WIFI_INTERFACE: [0-9]\+\s*\([0-9]\+\)\.\?/\1/" | grep -oP "^[0-9]+")
	noconnection=0
	
	if [[ $connection ]]; then
		color_side=#aaaaaa
	else 
		color_side=#ff2222
		noconnection=1
	fi

	let "connection = $connection * 10"
	connection=$(echo $connection | sed -r 's/\.[0-9]+//')
	connection=$(expr $connection / $total)
	off=$(expr $size - $connection)
	line="^p(_RIGHT)^p(-$location)^fg($color_side)$WIFI_INTERFACE: [^fg($color_con)"

	for i in `seq 1 $connection`;
	do
		line="${line}|"
	done

	line="${line}^fg($color_off)"

	for i in `seq $connection $size`;
	do
		line="${line}|"
	done

	line="${line}^fg($color_side)]"

	echo -ne "${line}^fg()"
}

volume() {
	color_vol=#444477
	color_off=#333333
	color_side=#aaaaaa
	location=500*16
	size=10
	speakericon=$SPEAKER
	line="^p(_RIGHT)^p(-$location)"

	if [[ $(amixer get Master | grep '\[off\]') ]]; then
		color_side=#ff2222
		speakericon=$MUTED3
		line="${line}^i($speakericon) Master: ^fg($color_side)["
		for i in `seq 1 $size`;
		do
			line="${line}^fg($color_off)|"
		done
		line="${line}^fg($color_side)]"
		echo -ne "${line}^fg()"
		return
	fi

	vol=$(amixer get Master | grep 'Front Left:' | grep -P -o "[0-9]+%" | sed 's/.$//')
	if [[ $vol ]]; then # if volume in config 2
		vol=$(expr $vol / $size)
		vol=$(echo $vol | sed -r 's/\.[0-9]+//') #Floor the volume ratio
		off=$(expr $size - $vol)
		
		line="${line}^i($speakericon) Master: ^fg($color_side)["
		for i in `seq 1 $vol`;
		do 
			line="${line}^fg($color_vol)|"
		done
		for i in `seq 1 $off`; do
			line="${line}^fg($color_off)|"
		done
		line="${line}^fg($color_side)]"
		echo -ne "${line}^fg()"
		return

	else # if no volume
		color_side=#ff2222
		line="${line}^fg($color_side)["
		for i in `seq 1 $size`;
		do
			line="${line}^fg($color_off)|"
		done
		line="${line}^fg($color_side)]"
		echo -ne "${line}^fg()"
		return
	fi
}

music() {
	state=$(mpc | grep -P -o "\[[a-z]+\]")
	location=250

	pausedcolor=#444477
	playingcolor=#9999ff

	line="^p(_LEFT)^p($location)"
	
	if [[ $state = "[playing]" ]]; then
		songinfo=$(mpc | head -n1)
		line="${line}^i($TREBLE) ^fg($playingcolor)${songinfo}^fg()"
	elif [[ $state = "[paused]" ]]; then
		songinfo=$(mpc | head -n1)
		line="${line}^i($PAUSED2) ^fg($pausedcolor)${songinfo}^fg()"
	else 
		line="${line}^i($STOPPED) No music playing"
	fi

	echo -ne "${line}^fg()"
}

logo() {
	fg_color=#ffff00
	echo -ne "^p(_LEFT)^fg($fg_color)^i($HWLOGO)^fg()"
}

battery() {
  cap=$(acpi -b | grep -oP "\d+%" | grep -oP "\d+")
	location=100*16

  is_charging=$(acpi -b | grep -oP "Charging" | xargs)

	if [[ -z $is_charging ]]; then
		echo "^p(_RIGHT)^p(-$location)^fg(#ff0000)[$cap%]^fg()"
	else
		echo "^p(_RIGHT)^p(-$location)^fg(#00aa00)[$cap%]^fg()"
	fi
}


while true; do
	clock
	workspaces
#	volume
	wifi
	music
	battery
	sleep 0.5
done | dzen2 -e - -h '20' -fn "-*-dejavu sans mono-medium-r-normal--*-100-*-*-*-*-iso10646-1" -w "${WIDTH}" -x "${X}" 
