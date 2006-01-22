# contents: zsh(1) user login RC.
#
# Copyright © 2004,2005,2006 Nikolai Weibull <now@bitwi.se>

keychain --dir $XDG_CACHE_HOME/keychain id_dsa
. $XDG_CACHE_HOME/keychain/$HOSTNAME-sh
