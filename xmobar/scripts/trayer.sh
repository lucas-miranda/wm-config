#!/bin/sh

# kill any leftover trayer processes
if [[ -n $(pgrep -u $UID -x trayer) ]]
then
    killall trayer
fi

# wait a little to ensure everything (at xmonad) is ready
sleep 1

# start trayer
trayer \
    --edge top \
    --align right \
    --SetDockType true \
    --SetPartialStrut true \
    --expand true \
    --height 20 \
    --widthtype request \
    --monitor 0 \
    --margin 0 \
    &
