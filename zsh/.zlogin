# contents: zsh(1) user login RC.
#
# Copyright © 2004,2005,2006 Nikolai Weibull <now@bitwi.se>

if whence keychain; then
  eval $(keychain --eval --quiet --dir $XDG_CACHE_HOME/keychain id_dsa)
fi
if [[ -z ${REMOTEHOST:-${SSH_CLIENT%% *}} ]] && whence ivman; then
  ivman -c $XDG_CONFIG_HOME/ivman &
fi
