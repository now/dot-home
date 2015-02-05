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
     [nweibull-ws|portair.local],
       [our_hostname=$enable_hostname_dotfiles],
     [AS_IF([test "x$enable_hostname_dotfiles_default" = xyes],
        [our_hostname=generic],
        [AC_MSG_FAILURE(
           [--enable-hostname-dotfiles for $enable_hostname_dotfiles is not supported])])])])
AC_ARG_WITH([dtachsession],
  [AS_HELP_STRING([--with-dtachsession],
     [override default dtach cache session file])],
  [dtachsession=$withval
   dtachsessiondir=`AS_DIRNAME(["$dtachsession"])`],
  [dtachsessiondir='${XDG_CONFIG_CACHE}/dtach'
   dtachsession='${dtachsessiondir}/session'])
AC_SUBST([dtachsessiondir])
AC_SUBST([dtachsession])
AC_ARG_WITH([zshistory],
  [AS_HELP_STRING([--with-zshistory], [override default Zsh history file])],
  [zshistory=$withval
   zshistorydir=`AS_DIRNAME(["$zshistory"])`],
  [zshistorydir='${XDG_CONFIG_CACHE}/zsh'
   zshistory='${zshistorydir}/history'])
AC_SUBST([zshistorydir])
AC_SUBST([zshistory])
AC_CHECK_PROGS([CURL], [curl])
AC_ARG_VAR([CURL], [the cURL command])
AC_CHECK_PROGS([DIFF], [diff])
AC_ARG_VAR([DIFF], [the diff command])
AC_CHECK_PROGS([DTACH], [dtach], [:])
AC_ARG_VAR([DTACH], [the dtach command])
AC_CHECK_PROGS([EMACS], [emacs])
AC_ARG_VAR([EMACS], [the Emacs editor command])
AC_CHECK_PROGS([ICONV], [iconv])
AC_ARG_VAR([ICONV], [the iconv command])
AC_PROG_INSTALL
AC_CHECK_PROGS([KEYCHAIN], [keychain], [:])
AC_ARG_VAR([KEYCHAIN], [the keychain command])
AC_PROG_MKDIR_P
AC_CHECK_PROGS([PATCH], [patch])
AC_ARG_VAR([PATCH], [the patch command])
AC_CHECK_PROGS([UNZIP], [unzip])
AC_ARG_VAR([UNZIP], [the unzip command])
AC_PATH_PROGS([ZSHELL], [zsh])
AC_ARG_VAR([ZSHELL], [the zsh command])
prefix=${HOME}
AC_ARG_VAR([XDG_CONFIG_CACHE], [the user’s cache directory])
: ${XDG_CONFIG_CACHE='${prefix}/.cache'}
AC_ARG_VAR([XDG_CONFIG_HOME], [the user’s configuration directory])
: ${XDG_CONFIG_HOME='${prefix}/.config'}
AC_CONFIG_LINKS(
  [bin/a.in:$os_bin_a_in
   bin/im.in:os/$our_host_os/bin/im.in
   host/host.mk:host/$our_hostname/host.mk
   libexec/clipboard-get.in:os/$our_host_os/libexec/clipboard-get.in
   libexec/clipboard-put.in:os/$our_host_os/libexec/clipboard-put.in
   os/os.mk:os/$our_host_os/os.mk])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
