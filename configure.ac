AC_PREREQ([2.69])
AC_INIT([dot-home], [1.0.0], [now@disu.se])
AC_CONFIG_SRCDIR([configure.ac])
AC_CONFIG_AUX_DIR([build])
AC_CANONICAL_HOST
AS_CASE([$host_os],
  [cygwin], [our_host_os=Cygwin],
  [darwin*], [our_host_os=Darwin],
  [linux-*], [our_host_os=Linux],
  [our_host_os=generic])
AS_CASE([$host_os],
  [darwin*], [os_bin_a_in=os/$our_host_os/bin/a.in],
  [os_bin_a_in=os/generic/bin/a.in])
AC_ARG_ENABLE([hostname-dotfiles],
  [AS_HELP_STRING([--enable-hostname-dotfiles@<:@=HOSTNAME@:>@],
     [enable hostname-specific dotfiles for HOSTNAME @<:@default=yes@:>@])],
  [],
  [enable_hostname_dotfiles=yes
   enable_hostname_dotfiles_default=yes])
AS_IF([test "x$enable_hostname_dotfiles" != xno],
  [AS_IF([test "x$enable_hostname_dotfiles" = xyes],
     [enable_hostname_dotfiles=`hostname`])
   AS_CASE([$enable_hostname_dotfiles],
     [portair.local],
       [our_hostname=$enable_hostname_dotfiles],
     [AS_IF([test "x$enable_hostname_dotfiles_default" = xyes],
        [our_hostname=generic],
        [AC_MSG_FAILURE(
           [--enable-hostname-dotfiles for $enable_hostname_dotfiles is not supported])])])])
AC_ARG_ENABLE([fonts],
  [AS_HELP_STRING([--enable-fonts],
     [enable installation of fonts])])
AC_SUBST([enable_fonts])
AC_ARG_WITH([awscredentials],
  [AS_HELP_STRING(
     [--with-awscredentials], [override default aws credentials file])],
  [awscredentials=$withval],
  [awscredentials='${XDG_CONFIG_HOME}/aws/credentials'])
AC_SUBST([awscredentials])
AC_ARG_WITH([dtachsession],
  [AS_HELP_STRING([--with-dtachsession],
     [override default dtach cache session file])],
  [dtachsession=$withval
   dtachsessiondir=`AS_DIRNAME(["$dtachsession"])`],
  [dtachsessiondir='${XDG_STATE_HOME}/dtach'
   dtachsession='${dtachsessiondir}/session'])
AC_SUBST([dtachsessiondir])
AC_SUBST([dtachsession])
AC_ARG_WITH([e-mail-address],
  [AS_HELP_STRING([--with-e-mail-address], [override default e-mail address])],
  [emailaddress=$withval],
  [emailaddress=now@disu.se])
AC_SUBST([emailaddress])
AC_ARG_WITH([gomodcachedir],
  [AS_HELP_STRING(
     [--with-gomodcachedir], [override default Go module cache directory])],
  [gomodcachedir=$withval],
  [gomodcachedir='${XDG_CACHE_HOME}/go/pkg/mod'])
AC_SUBST([gomodcachedir])
AC_ARG_WITH([hunspelldir],
  [AS_HELP_STRING([--with-hunspelldir], [override default hunspell directory])],
  [hunspelldir=$withval],
  [AS_CASE([$host_os],
     [darwin*], [hunspelldir='${prefix}/Library/Spelling'],
     [hunspelldir='${userconfdir}/.openoffice.org/3/user/wordbook'])])
AC_SUBST([hunspelldir])
AC_ARG_WITH([m2localrepositorydir],
  [AS_HELP_STRING(
     [--with-m2localrepositorydir],
     [override default m2 local repository directory])],
  [m2localrepositorydir=$withval],
  [m2localrepositorydir='${XDG_CACHE_HOME}/maven'])
AC_SUBST([m2localrepositorydir])
AC_ARG_WITH([projectsdir],
  [AS_HELP_STRING([--with-projectsdir], [override default projects directory])],
  [projectsdir=$withval],
  [projectsdir='${prefix}/Projects'])
AC_SUBST([projectsdir])
AC_ARG_WITH([yarncachedir],
  [AS_HELP_STRING(
     [--with-yarncachedir], [override default yarn cache directory])],
   [yarncachedir=$withval],
   [yarncachedir='${XDG_CACHE_HOME}/yarn'])
AC_SUBST([yarncachedir])
AC_ARG_WITH([yarnstatedir],
  [AS_HELP_STRING(
     [--with-yarnstatedir], [override default yarn state directory])],
   [yarnstatedir=$withval],
   [yarnstatedir='${XDG_STATE_HOME}/yarn'])
AC_SUBST([yarnstatedir])
AC_ARG_WITH([zshcachedir],
  [AS_HELP_STRING(
     [--with-zshcachedir], [override default Zsh cache directory])],
  [zshcachedir=$withval],
  [zshcachedir='${XDG_CACHE_HOME}/zsh'])
AC_SUBST([zshcachedir])
AC_ARG_WITH([zshcompletioncachedir],
  [AS_HELP_STRING(
    [--with-zshcompletioncachedir],
    [override default Zsh completion cache directory])],
  [zshcompletioncachedir=$withval],
  [zshcompletioncachedir='${zshcachedir}/completion'])
AC_SUBST([zshcompletioncachedir])
AC_ARG_WITH([zshcompletiondump],
  [AS_HELP_STRING(
    [--with-zshcompletiondump], [override default Zsh completion dump file])],
  [zshcompletiondump=$withval
   zshcompletiondumpdir=`AS_DIRNAME(["$zshcompletiondump])`],
  [zshcompletiondumpdir='${zshcachedir}'
   zshcompletiondump='${zshcompletiondumpdir}/completion-dump'])
AC_SUBST([zshcompletiondumpdir])
AC_SUBST([zshcompletiondump])
AC_ARG_WITH([zshistory],
  [AS_HELP_STRING([--with-zshistory], [override default Zsh history file])],
  [zshistory=$withval
   zshistorydir=`AS_DIRNAME(["$zshistory"])`],
  [zshistorydir='${zshcachedir}'
   zshistory='${zshistorydir}/history'])
AC_SUBST([zshistorydir])
AC_SUBST([zshistory])
AC_CHECK_PROGS([CURL], [curl])
AC_ARG_VAR([CURL], [the cURL command])
AC_CHECK_PROGS([DIFF], [diff])
AC_ARG_VAR([DIFF], [the diff command])
AC_CHECK_PROGS([DTACH], [dtach], [:])
AC_ARG_VAR([DTACH], [the dtach command])
AC_PATH_PROGS([EMACS], [Emacs emacs emacs-nox], [],
  [/Applications/MacPorts/Emacs.app/Contents/MacOS$PATH_SEPARATOR$PATH])
AC_ARG_VAR([EMACS], [the Emacs editor command])
AC_PATH_PROGS([EMACSCLIENT], [emacsclient emacsclient-nox], [],
  [/Applications/MacPorts/Emacs.app/Contents/MacOS/bin$PATH_SEPARATOR$PATH])
AC_ARG_VAR([EMACSCLIENT], [the emacsclient command])
AC_CHECK_PROGS([EXIFTOOL], [exiftool exiftool-5.22])
AC_ARG_VAR([EXIFTOOL], [the exiftool command])
AC_CHECK_PROGS([GREP], [ggrep grep])
AC_ARG_VAR([GREP], [the grep command])
AC_CHECK_PROGS([GZIP], [gzip])
AC_ARG_VAR([GZIP], [the gzip command])
AC_CHECK_PROGS([ICONV], [iconv])
AC_ARG_VAR([ICONV], [the iconv command])
AC_PROG_INSTALL
AC_CHECK_PROGS([KEYCHAIN], [keychain], [:])
AC_ARG_VAR([KEYCHAIN], [the keychain command])
AC_CHECK_PROGS([LAUNCHCTL], [launchctl], [:])
AC_ARG_VAR([LAUNCHCTL], [the launchctl command])
AC_PROG_MKDIR_P
AC_CHECK_PROGS([MKTEMP], [gmktemp mktemp])
AC_ARG_VAR([MKTEMP], [the mktemp command])
AC_CHECK_PROGS([PATCH], [patch])
AC_ARG_VAR([PATCH], [the patch command])
AC_CHECK_PROGS([PORT], [port])
AC_ARG_VAR([PORT], [the port command])
AC_PATH_PROGS([SOCAT], [socat])
AC_ARG_VAR([SOCAT], [the socat command])
AC_CHECK_PROGS([SUDO], [sudo])
AC_ARG_VAR([SUDO], [the sudo command])
AC_CHECK_PROGS([UNZIP], [unzip])
AC_ARG_VAR([UNZIP], [the unzip command])
AC_CHECK_PROGS([XCLIP], [xclip])
AC_ARG_VAR([XCLIP], [the xclip command])
AC_CHECK_PROGS([XSLTPROC], [xsltproc])
AC_ARG_VAR([XSLTPROC], [the xsltproc command])
AC_PATH_PROGS([ZSHELL], [zsh])
AC_ARG_VAR([ZSHELL], [the zsh command])
prefix=${HOME}
exec_prefix='${prefix}/.local'
datarootdir='${XDG_STATE_HOME}'
AC_ARG_VAR([XDG_CACHE_HOME], [the user’s cache directory])
: ${XDG_CACHE_HOME='${prefix}/.cache'}
AC_ARG_VAR([XDG_CONFIG_HOME], [the user’s configuration directory])
: ${XDG_CONFIG_HOME='${prefix}/.config'}
AC_ARG_VAR([XDG_DATA_HOME], [the user’s data directory])
: ${XDG_DATA_HOME='${prefix}/.local/share'}
AC_ARG_VAR([XDG_STATE_HOME], [the user’s state directory])
: ${XDG_STATE_HOME='${prefix}/.local/state'}
AC_CONFIG_LINKS(
  [bin/a.in:$os_bin_a_in
   bin/im.in:os/$our_host_os/bin/im.in
   host/host.mk:host/$our_hostname/host.mk
   libexec/clipboard-get.in:os/$our_host_os/libexec/clipboard-get.in
   libexec/clipboard-put.in:os/$our_host_os/libexec/clipboard-put.in
   os/os.mk:os/$our_host_os/os.mk])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
