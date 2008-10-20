#! /bin/bash
# contents: X11 session initialization.
#
# Copyright © 2007 Nikolai Weibull

# Find a file we can redirect output to.
for errfile in "$HOME/.local/var/log/x11-session-errors" \
               "${TMPDIR-/tmp}/x11-session-$USER" \
               "/tmp/x11-session-$USER"; do
  if ( cp /dev/null "$errfile" 2> /dev/null ); then
    chmod 600 "$errfile"
    exec > "$errfile" 2>&1
    break
  fi
done

if ( cp /dev/null "$HOME/.xsession-errors" 2> /dev/null ); then
  rm "$HOME/.xsession-errors"
fi

userfonts=$HOME/.local/share/fonts
userresources=$HOME/.local/etc/X11/Xresources

# Add directory where additional X11 fonts can be found.
if [ -d "$userfonts" ]; then
  xset +fp "$userfonts"
fi

# Merge user’s resource file into the X session.
if [ -f "$userresources" ]; then
  xrdb -merge "$userresources"
fi

# Set keyboard and mouse rates.
xset r rate 240 30 m 2 20

# Set a nice color for the root window.
xsetroot -solid "#4b5b67"

# Fix the Windows key (why this needs to be done I’ll never know).
setxkbmap -option 'win:meta'

# Start programs running in background.
gnome-terminal -e 'sh -c "TERM=xterm-256color screen -d -RR"' &
(sleep 2; firefox) &

# Start xmonad(1).
xmonad
