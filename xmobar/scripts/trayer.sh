#!/bin/sh

# kill any leftover trayer processes
if [[ -n $(pgrep -u $UID -x trayer) ]]
then
    killall trayer
fi

# wait a little to ensure everything (at xmonad) is ready
sleep 3

# start trayer
trayer \
    --edge top \
    --align right \
    --SetDockType true \
    --SetPartialStrut true \
    --expand true \
    --distancefrom top \
    --distance 1 \
    --widthtype request \
    --height 16 \
    --monitor 0 \
    --margin 0 \
    --transparent true \
    --alpha 0 \
    --tint $1 \
    &

