#!/bin/bash

if [ -f ~/.xmonad/trayer.pid ]; then
    TRAYER_PID=$(cat ~/.xmonad/trayer.pid)
    echo "b $TRAYER_PID $(ps -p $TRAYER_PID -o comm=)"
    if [ "$(ps -p $TRAYER_PID -o comm=)" == "trayer" ]; then
        kill $TRAYER_PID
    fi
fi

trayer --edge top --align right --SetDockType true --SetPartialStrut true \
       --expand true --width 6 --transparent true --tint 0x191970 --height 14 &
echo "$!" > ~/.xmonad/trayer.pid

feh --bg-scale Pictures/wallpapers/cradle2k31600.jpg &
