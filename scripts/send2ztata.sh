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
		  echo "found comeback flag";;
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
   # all good; just checking syntax
   ;;
   * )
	  Usage
	  exit 666;;
esac

numstatas=$(pgrep -c stata)
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

## loop would assign here
theStata=$allStatas

# get name of Emacs window
#   comment out for command-line debugging
winid=$(xdotool getactivewindow getwindowname)

# Check to see if it is a windowed Stata or an old-school Stata
if [ $(pstree $theStata | wc -l) -eq 1 ]; then
	## cannot paste to old-school Stata because the window name
	## is not distinctive without some hacks
	echo "Cannot paste to non-GUI Stata's yet"
	exit 4
else
   ## not finding consoles because there is no consistent name
   xdotool search --name --onlyvisible "Stata(/SE|/MP)* 1[1-5]" windowactivate
fi

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

