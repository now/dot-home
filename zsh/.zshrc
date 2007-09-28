# contents: zsh(1) user RC.
#
# Copyright © 2002,2003,2004,2005,2006 Nikolai Weibull <now@bitwi.se>

# 1  Parameters {{{1
# =============


HISTFILE=~/.local/var/lib/zsh/history
HISTSIZE=1024
READNULLCMD=$PAGER
REPORTTIME=300
SAVEHIST=$HISTSIZE
watch=(notme)
unset BAUD



# 2  Limits {{{1
# =========


limit coredumpsize  30m



# 3  Shell Settings {{{1
# =================


hash -d ned=~/src/\{projects\}/ned
hash -d context=/usr/local/share/texmf/tex/context
hash -d mycontext=~/.local/share/texmf/tex/context

zmodload -i zsh/parameter
sleep 0
[[ -n $(whence script) ]] && unhash script


# 4  Shell Options {{{1
# ================

# magicequalsubst 
setopt autocd                                 \
       autopushd                              \
       completeinword nolistambiguous         \
       extendedhistory                        \
       rcexpandparam rcquotes                 \
       correct dvorak                         \
       nonotify                               \
       nobeep



# 5  Terminal Settings {{{1
# ====================


stty -ixon                    # turn off flowcontrol for the terminal
export TERMCAP=



# 5.1. Screen Title Updating
# --------------------------

_set_title () {
  [[ $LOCKTITLE == 1 ]] && return
  case $TERM in
    (screen*)
      print -nR $'\ek'${1[1,18]}$'\e\\' ;;
  esac
  return 0
}

precmd () {
  _set_title zsh
}

preexec () {
  local -a cmd
  
  cmd=(${(z)1})
  case $cmd[1] in
    (fg)
      if (( $#cmd == 1 )); then
        cmd=(builtin jobs -l %+)
      else
        cmd=(builtin jobs -l ${(Q)cmd[2]})
      fi ;;
    (r)
      cmd=($(builtin history -n -1))
      _set_title $cmd[1]:t
      return ;;
    (%*)
      cmd=(builtin jobs -l ${(Q)cmd[1]}) ;;
    (exec)
      shift cmd ;&
    (*)
      _set_title $cmd[1]:t
      return ;;
  esac

  local -A jt

  jt=(${(kv)jobtexts})

  $cmd >>(read num rest
          cmd=(${(z)${(e):-\$jt$num}})
          _set_title $cmd[1]:t) 2>/dev/null
}



# 6  Modules {{{1
# ==========


zmodload -a zsh/stat stat
zmodload -a zsh/zprof zprof
zmodload -i zsh/complist



# 7  Command-line Completion {{{1
# ==========================


autoload -U compinit; compinit -d ~/.local/var/cache/zsh/compdump


# 7.1. Completion Styles {{{2
# ----------------------

zstyle '*' hosts remote1.tekno.chalmers.se \
  panini.cling.gu.se vetlanda.tekno.chalmers.se \
  shell.scorpionshops.com ${REMOTEHOST:-${SSH_CLIENT%% *}}

zstyle ':completion:*' squeeze-slashes yes

zstyle ':completion::complete:*' use-cache yes
zstyle ':completion::complete:*' cache-path $XDG_CACHE_HOME/zsh/completion-$HOST

zstyle ':completion:::::' completer _expand_alias _expand _complete _prefix _ignored
zstyle ':completion::prefix:::' completer _complete

zstyle ':completion:*' expand prefix suffix
#zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' matcher-list 'm:{a-zA-Z}={A-Za-z}' 'r:|[:.,_-]=** r:|=**'
zstyle ':completion:*:*:-subscript-:*' tag-order indexes parameters
zstyle ':completion:*:expand:*' tag-order all-expansions

zstyle ':completion::*:(-command-|export):*' fake-parameters \
  ACCEPT_KEYWORDS:scalar USE:scalar
zstyle ':completion:*:ignored:*' single-ignored menu

zstyle ':completion:*:*:kill:*' menu yes select
zstyle ':completion:*:kill:*' force-list always
zstyle ':completion:*:processes' command 'ps -U ${EUID} 2>/dev/null'

zstyle '*:users' ignored-patterns \
  adm alias apache at bin cl-builder cron cvs cyrus daemon ftp ftpuser games \
  games-ded gdm guest haldaemon halt ldap lp mail man messagebus mserv mysql \
  named news nobody ntp nut operator portage postfix postgres postmaster \
  privoxy qmaild qmaill qmailp qmailq qmailr qmails rpc shutdown smmsp squid \
  sshd sync uucp vpopmail xfs
zstyle ':completion:*:functions' ignored-patterns '_*'
zstyle ':completion:*:*:(^rm):*:*files' ignored-patterns \
  '*?.o' '*?.~*~' '*?.class' '*?.obj' '*?.sw[po]'

zstyle ':completion:*' ignore-parents parent pwd
zstyle ':completion:*:rm:*' ignore-line yes


# 7.2. Completion Formats and Messages {{{2
# ------------------------------------

if [[ -x $(whence dircolors) && -r ~/.local/etc/dircolors ]]; then
  eval $(dircolors ~/.local/etc/dircolors)
fi

zstyle ':completion:*:default' list-colors ${(s.:.)LS_COLORS} tc=7

zstyle ':completion:*' verbose yes
zstyle ':completion:*' list-separator "                "
zstyle ':completion:*:options' auto-description "specify ‘%d’"

zstyle ':completion:*:*:kill:*:processes' list-colors '=(#b) #([0-9]#)*=8=9'

zstyle ':completion:*:directory-stack' list-colors "=(#b)[0-9]#[[:space:]]#(*)=8=${${LS_COLORS#*di=}%%:*}"


# 7.3. Completion Groups {{{2
# ----------------------

zstyle ':completion:*' group-name ''
zstyle ':completion:*:manuals' separate-sections true


# 7.4. History Completion {{{2
# -----------------------

zstyle ':completion:*:history-words' stop yes
zstyle ':completion:*:history-words' remove-all-dups yes
zstyle ':completion:*:history-words' list no
zstyle ':completion:*:history-words' menu yes


# }}}2



# 8  ZLE Keybindings {{{1
# ==================


# 8.1  VI Command Mode {{{2
# --------------------

# 8.1.1  Movement

bindkey -a "t" down-line-or-history
bindkey -a "n" up-line-or-history
bindkey -a "s" vi-forward-char

autoload -U forward-word-match
autoload -U backward-word-match
zle -N forward-parameter forward-word-match
zle -N backward-parameter backward-word-match
zstyle ':zle:forward-parameter' word-style shell
zstyle ':zle:backward-parameter' word-style shell
bindkey -a ")" forward-parameter
bindkey -a "(" backward-parameter

# 8.1.2  Searching

bindkey -a "/" history-incremental-search-backward
bindkey -a "?" history-incremental-search-forward
bindkey -a "j" vi-find-next-char-skip
bindkey -a "J" vi-find-prev-char-skip
bindkey -a "k" vi-repeat-search
bindkey -a "K" vi-rev-repeat-search 

# 8.1.3  Change

bindkey -a "L" vi-change-whole-line
bindkey -a "T" vi-join
bindkey -a "l" vi-substitute

# 8.1.4  Miscellaneous

bindkey -arp "^["
bindkey -ar "#"
bindkey -ar "+"
bindkey -ar "\-"
bindkey -ar "^?"
bindkey -a "^[" vi-cmd-mode
bindkey -a "^L" accept-line-and-down-history
bindkey -a "^R" redo
bindkey -a "N" run-help
bindkey -a "u" undo
bindkey -a "~" vi-oper-swap-case

autoload -Uz replace-string
zle -N replace-pattern replace-string
bindkey -a ":s" replace-pattern


# 8.2  VI Insert Mode {{{2
# -------------------

# 8.2.1  Completion

bindkey "^D" list-choices
bindkey "^I" complete-word
bindkey "^P" _history-complete-older
bindkey "^N" _history-complete-newer
bindkey "^U" undo

autoload -Uz insert-digraph
zle -N insert-digraph
bindkey "^K" insert-digraph

# 8.2.4  Miscellaneous

_complete-previous-output () {
  compadd - ${(f)"$(eval $(fc -l -n -1))"}
}
zle -C complete-previous-output complete-word _complete-previous-output

source $fpath/keeper(N)
_complete-kept () {
  compadd -a kept
}
zle -C complete-kept complete-word _complete-kept

bindkey -r "^O"
bindkey -rp "^X"
bindkey -r "^X"
bindkey -r "^?"
bindkey -rp "^["
bindkey "^O$" insert-last-word
bindkey "^Ok" complete-kept
bindkey "^O^k" complete-kept
bindkey "^Om" _most_recent_file
bindkey "^Oo" complete-previous-output
bindkey "^O^O" complete-previous-output
bindkey "^Op" copy-prev-shell-word
bindkey "^O?" _complete_help                  
bindkey "^E" list-expand
bindkey "^F" describe-key-briefly
bindkey "^C" vi-cmd-mode
bindkey "^L" accept-line-and-down-history
bindkey "^R" redisplay
bindkey "^Q" push-line-or-edit
bindkey " " magic-space
bindkey "^H" backward-delete-char
bindkey "^?" backward-delete-char

autoload -U history-beginning-search-menu
zle -N history-beginning-search-menu
bindkey "^X^L" history-beginning-search-menu

autoload -U backward-kill-word-match
zle -N backward-kill-to-space-or-/ backward-kill-word-match
zstyle ':zle:backward-kill-to-space-or-/' word-chars '*?_-.[]~=&;!#$%^(){}<>:@,\\'
bindkey "^W" backward-kill-to-space-or-/

_sudo-command-line () {
  [[ $BUFFER != sudo\ * ]] && LBUFFER="sudo $LBUFFER"
}
zle -N sudo-command-line _sudo-command-line
bindkey "^Os" sudo-command-line

_paste-x11-clipboard () {
  [[ -x $(whence xclip) ]] || return
  LBUFFER+=$(xclip -out)
}
zle -N paste-x11-clipboard _paste-x11-clipboard
bindkey "^Y" paste-x11-clipboard

self-insert-redir () {
  integer len=$#LBUFFER
  zle self-insert
  (( $len >= $#LBUFFER )) && LBUFFER[-1]=" $LBUFFER[-1]"
}
zle -N self-insert-redir
bindkey ${(s: :):-${^${(s::):-"|<>&"}}" self-insert-redir"}


# 8.3  List-scroll Mode {{{2
# ---------------------

bindkey -M listscroll 't' down-line-or-history
bindkey -M listscroll 'q' send-break


# }}}2



# 9  Function Autoloading {{{1
# =======================


autoload -U promptinit; promptinit
prompt now

autoload -U cdup d zcalc

autoload -U zmv
alias mmv='noglob zmv -W'

zstyle ':mime:*' mailcap $XDG_CONFIG_HOME/mailcap
autoload -U zsh-mime-setup; zsh-mime-setup

autoload -U zargs



# 10  Aliases {{{1
# ===========


# 10.1  Convenience Aliases/Macros {{{2
# --------------------------------

#alias conkeror='firefox -chrome chrome://conkeror/content'
alias v='vim'
alias ro='vimless'
alias view='vimless'
e () {
  if vim --serverlist | grep -q '^VIM$'; then
    if (( $# )); then
      vim --servername vim --remote $@
    fi
    screen -X select 6
  else
    screen -t 'edit' 6 vim --servername vim $@
  fi
}

#alias vr='vim --remote'
#es () {
#  if vim --serverlist | grep '^VIM$' >& /dev/null; then
#    vim --servername vim --remote $@
#  else
#    screen -t 'edit' 6 vim --servername vim -S $@
#  fi
#}
#:e () {
#  if vim --serverlist | grep -q '^VIM$'; then
#    vim --servername vim --remote $@
#    exit
#  else
#    vim --servername vim $@
#  fi
#}

alias mv='nocorrect mv'
alias cp='nocorrect cp'
alias mkdir='nocorrect mkdir'
alias duh='du -ch --max-depth=1'  # run `du' with human readable, short output
alias dfh='df -h'               # run `df' with human readable output
                                # run `ps' with nice output format
alias p='ps --User $USERNAME --format "pid comm" -H'
aspell () { command aspell --home-dir ~/.local/var/lib/aspell $* }
sbcl () { command sbcl $* --userinit ~/.local/etc/sbclrc }
                                # enable grep color output and print linenumber
alias gp='grep --color=auto -n -P'
alias top='top -u $USERNAME'    # run top with default user
alias mp='mplayer'
alias mpq='mplayer -nosound'
alias tv='DISPLAY=:0.1 mplayer -vo gl2 -dr'
alias ri='noglob ri'

alias -g ...='../..'
alias -g ....='../../..'
alias -g .....='../../../..'

cd () {
  if [[ -f $1 ]]; then
    builtin cd $1:h
  else
    builtin cd $1 |& \
      sed -n \
      's/^cd:cd:4: \([^:]\+\): \(.*\)$/'"$0: can't change directory (\1):\n\
    dir: \2/p"
    (( $pipestatus[0] == 0 )) && builtin cd $1
  fi
}

vg () {
  if (( $# < 2 )); then
    print -u 2 "Usage: $0 PATTERN FILE..."
    return 1
  fi
  local regex=$1; shift
  vim \
      -c 'let saved_grepprg = &grepprg' \
      -c 'set grepprg=pcregrep\ -u\ -n\ --\ $*\ /dev/null' \
      -c 'let saved_shellpipe = &shellpipe' \
      -c 'set shellpipe=>\ %s\ 2>&1' \
      -c "silent! grep '${regex//(#m)[%#]/\\$MATCH}' ${^${(@qq)${(@)*//(#m)[%#]/\\$MATCH}}}" \
      -c 'let &shellpipe = saved_shellpipe' \
      -c 'unlet saved_shellpipe' \
      -c 'let &grepprg = saved_grepprg' \
      -c 'unlet saved_grepprg' \
      -c 'if len(filter(getqflist(), "v:val.valid")) == 0 | quit | else | cwindow | endif' \
}


# 10.2  ls Aliases {{{2
# ----------------

alias ls='ls --color=auto'
alias a='ls'
alias o='ls -lh'
alias aa='a -A'
alias oo='o -A'
i () { command ls -A -l --color=always -b "$@" | ${LISTER:-less} }


# 10.3  Shell Aliases {{{2
# -------------------

freload () {
  while (( $# )); do
    unfunction $1
    autoload -U $1
    shift
  done
}


# }}}2



# }}}1
