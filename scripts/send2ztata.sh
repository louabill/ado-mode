#!/bin/bash
# original idea by Helge Liebert
# heavily modified for ado-mode by Bill Rising
# strange name because 'send2stata' would be a process with 'stata' in its name

# script needs xsel and xdotool 
# to get them run
# in debian/ubuntu linux
# sudo apt-get install xsel xdotool
# in arch
# sudo pacman -S xsel xdotool
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

while getopts ":acd:f:h" opt; do
   case $opt in
	  a ) send2all="send2all";;
	  c ) comeback="comeback"
		  # echo "found comeback flag"
		  ;;
	  d ) dothis="$OPTARG";;
	  f ) flavor="$OPTARG";;
      h ) Usage
		  exit 0;;
      \? ) Usage
           exit 0;;
      * ) Usage 
          exit 55;;
   esac
done

shift $(($OPTIND -1))

case $dothis in
   command | dofile | include | menu )
   # all good; just checking syntax
   ;;
   * )
	  Usage
	  exit 666;;
esac

numstatas=$(pgrep -c xstata)
if [ $numstatas -eq 0 ]; then
   echo "No Stata open!"
   exit 2
fi

## multiple Statas could loop through the pid's
###  starting small for now
if [ $numstatas -ge 2 ]; then
	echo "Found $numstatas Stata processes...."
	echo "...nothing for multiple Statas yet"
	exit 3
fi

## only works for windowed versions of Stata
allStatas=$(pgrep -l xstata | sed -E 's/[0-9]+ //')

## loop would assign here; all
theStata=$allStatas

winid=$(xdotool getactivewindow getwindowname)
## echo "*** winid is: $winid ***"

## commented out checking for windowed statas, as that is all that
## the pgrep should find
# Check to see if it is a windowed Stata or an old-school Stata
# if [ $(grep $theStata | wc -l) -eq 1 ]; then
# 	## cannot paste to old-school Stata because the window name
# 	## is not distinctive without some hacks
# 	echo "Cannot paste to non-GUI Stata's yet"
# 	exit 4
# else
## not finding consoles because there is no consistent name
## ok, finally gave up and changed regexp to match Stata 10-29, so this
##  should work until 2045
   xdotool search --name --onlyvisible "Stata(/SE|/MP|/IC/BE)* [12][0-9]" windowactivate
# fi							   

## make do-file if dothis is anything but command
## using case for easier future maintenance

case $dothis in
   command )
	  # get to command window and select all
	  xdotool key ctrl+1
	  xdotool key ctrl+a
	  # paste in the clipboard
	  xdotool key ctrl+v
	  ;;
   dofile | include | menu )
	  # paste clipboard to a tmp dofile
	  xsel -b -o > ${tmpDir}${tmpDoFile}
	  sleep 0.2
	  # end eol
	  sed -i -e '$a\' ${tmpDir}${tmpDoFile}
	  case $dothis in
		 dofile )
			xdotool type do " ${tmpDir}$tmpDoFile"
			;;
		 include )
			xdotool type include " ${tmpDir}$tmpDoFile"
			;;
		 else )
			echo "$dothis not implemented for do-files in unix"
			;;
	  esac
	  ;;
   else )
		   echo "$dothis not implemented, somehow"
		   ;;
esac

# hit Return
xdotool key Return

## back to Emacs
if [ -n "$comeback" ]; then
	xdotool search --name --onlyvisible "$winid" windowactivate
fi

