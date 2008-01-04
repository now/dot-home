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
ratpoisonrc=$HOME/.local/etc/ratpoisonrc
background=$HOME/media/images/backgrounds/OTHER-linux_1600.png

# Add directory where additional X11 fonts can be found.
if [ -d "$userfonts" ]; then
  xset +fp "$userfonts"
fi

# Merge user’s resource file into the X session.
if [ -f "$userresources" ]; then
  xrdb -merge "$userresources"
fi

# Start a DBUS session.
if [ -x /etc/X11/xinit/xinitrc.d/30-dbus ]; then
  . /etc/X11/xinit/xinitrc.d/30-dbus
fi

# Set keyboard and mouse rates.
xset r rate 240 30 m 2 20

# Set the cursor of the root and a default color.
xsetroot -cursor_name left_ptr -solid "#a4b0c0"

# Set a background for root.
xsetbg -center "$background"

setxkbmap -option 'win:meta'

# Start programs running in background.
#unrat &
gnome-terminal -e 'sh -c "TERM=xterm-256color screen -d -RR"' &
(sleep 2; firefox) &

# Start xmonad(1).
xmonad
