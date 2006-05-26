# contents: zsh(1) user profile RC.
#
# Copyright © 2002,2003,2004,2005,2006 Nikolai Weibull <now@bitwi.se>

declare -U path
path=(. ~/bin $path)

declare -TU MAILCAPS mailcaps
mailcaps=(~/.local/etc/mailcap)
PERSONAL_MAILCAP=~/.local/etc/mailcap

LANG=en_US.UTF-8
LC_COLLATE=C

PAGER=vimless
MANPAGER="col -b | vimless +'setf man' -"
EDITOR=vim
VERSION_CONTROL=numbered
VERSION_WIDTH=2
GREP_COLOR="01;32"
EMAIL="Nikolai Weibull <now@bitwi.se>"

MANWIDTH=79

XDG_CONFIG_HOME=~/.local/etc
XDG_CACHE_HOME=~/.local/var/cache
MONO_SHARED_DIR=~/.local/var/run/mono
GEMCACHE=$XDG_CACHE_HOME/rubygems

VIMINIT="so ~/.local/etc/vim/vimrc"
XAUTHORITY=~/.local/var/lib/misc/Xauthority
ICEAUTHORITY=~/.local/var/lib/misc/ICEauthority
CVS_PASSFILE=$XDG_CACHE_HOME/cvs/cvspass
INPUTRC=$XDG_CONFIG_HOME/inputrc
INDENT_PROFILE=$XDG_CONFIG_HOME/indentrc
SCREENRC=$XDG_CONFIG_HOME/screenrc
GST_REGISTRY=$XDG_CACHE_HOME/gst/registry.xml
GNUPGHOME=$XDG_CONFIG_HOME/gnupg
IRBRC=$XDG_CONFIG_HOME/irbrc
LFTP_HOME=$XDG_CONFIG_HOME/lftp
WINEPREFIX=~/.local/lib/wine
MPLAYER_HOME=$XDG_CONFIG_HOME/mplayer
GTK2_RC_FILES=$XDG_CONFIG_HOME/gtkrc
LESSHISTFILE=~/.local/var/lib/less/history
GIMP2_DIRECTORY=.local/var/lib/gimp-2.0
GNOME22_USER_DIR=~/.local/var/lib/gnome2

export                                                                \
  PATH MAILCAPS PERSONAL_MAILCAP                                      \
  LANG LC_COLLATE                                                     \
  PAGER MANPAGER EDITOR                                               \
  VERSION_CONTROL VERSION_WIDTH GREP_COLOR EMAIL                      \
  MANWIDTH                                                            \
  XDG_CONFIG_HOME XDG_CACHE_HOME MONO_SHARED_DIR GEMCACHE             \
  VIMINIT XAUTHORITY ICEAUTHORITY CVS_PASSFILE INPUTRC INDENT_PROFILE \
  SCREENRC GST_REGISTRY GNUPGHOME IRBRC LFTP_HOME WINEPREFIX          \
  MPLAYER_HOME GTK2_RC_FILES LESSHISTFILE GIMP2_DIRECTORY             \
  GNOME22_USER_DIR
