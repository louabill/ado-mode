#!/bin/bash
# original idea by Helge Liebert
# heavily modified for ado-mode by Bill Rising

# script needs wmctrl, xte, xsel and xdotool 
# to get them run
# in debian/ubuntu linux
# sudo apt-get install wmctrl xautomation xsel xdotool
# in arch
# sudo pacman -S wmctrl xautomation xsel xdotool
# the default delays should work on most systems
self=$(basename $0)
# set defaults (none)
tmpDir="/tmp/"
tmpDoFile="feedStata.do"

Usage () {
   cat <<EOF 1>&2
$self -d dothis [ -t tmpdir ]

General Options

Help Options
-h prints this help
-? prints this help
EOF
}

while getopts ":ad:f:h" opt; do
   case $opt in
	  a ) ="send2all";;
	  d ) dothis="$OPTARG";;
	  f ) flavor="$OPTARG";;
      h ) Usage
		  exit 0;;
      \? ) Usage
           exit 0;;
      * ) Usage 
          exit 1;;
   esac
done

shift $(($OPTIND -1))

case $dothis in
   command | dofile | include | menu )
	  # all good
	  ;;
   else )
		   Usage
		   exit 666;;
esac

theStatas=$(pgrep stata)
## check how many Stata's are running
numstatas=$(echo $theStatas | wc -l)
if [ $numstatas .eq 0 ]; then
   echo "No Stata open!"
   exit 2
fi

## multiple Statas could loop through the pid's
###  starting small for now
if [ $numstatas .ge 1 ]; then
	echo "Nothing for multiple Stata's yet"
	exit 3
fi

## loop would assign here
theStata=$allStatas

# get name of Emacs window
#   comment out for command-line debugging
#  winid=$(xdotool getactivewindow getwindowname)

# Check to see if it is a windowed Stata or an old-school Stata
if [ $(pstree theStata) .eq 1 ]; then
	## cannot paste to old-school Stata because the window name
	## is not distinctive without some hacks
	echo "Cannot paste to non-GUI Stata's yet"
	exit 4
else
   wmctrl -a Stata
fi

## make do-file if dothis is anything but command
## using case for easier future maintenance

case $dothis in
   command )
	  xte 'keydown Control_L' 'key 1' 'key A' 'usleep 100' \
		  'key V' 'keyup Control_L'
	  ;;
   dofile | include | menu )
	  # paste clipboard to a tmp dofile
	  xsel -o > ${tmpDir}${tmpDoFile}
	  case $dothis in
		 dofile )
			xte "str do ${tmpDir}$tmpDoFile"
			;;
		 include )
			xte "str include ${tmpDir}$tmpDoFile"
			;;
		 else )
			echo "$dothis not implemented for do-files"
			;;
	  esac
	  ;;
   else )
		   echo "$dothis not implemented, somehow"
		   ;;
esac

sleep .1
xte 'key Return'

## 
## sleep .3

# go back to editor window
#   comment out for command-line debugging
# wmctrl -a $winid 


