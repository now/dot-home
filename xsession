#! /bin/bash
# contents: X11 session initialization.
#
# Copyright © 2007 Nikolai Weibull

# Set keyboard and mouse rates.
xset r rate 240 30 m 2 20

# Set a nice color for the root window.
xsetroot -cursor_name left_ptr -solid "#4b5b67"

# Fix the Windows key (why this needs to be done I’ll never know).
setxkbmap -option 'win:meta'

# Start programs running in background.
gnome-terminal -e 'sh -c "TERM=xterm-256color screen -d -RR"' &
(sleep 2; firefox) &

# Start xmonad(1).
xmonad
