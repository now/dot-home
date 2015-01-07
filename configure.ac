AC_PREREQ([2.69])
AC_INIT([dot-home], [1.0.0], [now@disu.se])
AC_CONFIG_SRCDIR([configure.ac])
AC_CONFIG_AUX_DIR([build])
AC_CANONICAL_HOST
AC_SUBST([os_mk], [/dev/null])
AS_CASE([$host_os],
  [cygwin], [os_mk=$srcdir/os/Cygwin/os.mk],
  [darwin*], [os_mk=$srcdir/os/Darwin/os.mk],
  [linux-*], [os_mk=$srcdir/os/Linux/os.mk])
AC_SUBST([host_mk], [/dev/null])
AC_ARG_ENABLE([hostname-dotfiles],
  [AS_HELP_STRING([--enable-hostname-dotfiles@<:@=HOSTNAME@:>@],
     [enable hostname-specific dotfiles for HOSTNAME @<:@default=yes@:>@])],
  [],
  [enable_hostname_dotfiles=yes])
AS_IF([test "x$enable_hostname_dotfiles" != xno],
  [AS_IF([test "x$enable_hostname_dotfiles" = xyes],
     [enable_hostname_dotfiles=`hostname`])
   AS_CASE([$enable_hostname_dotfiles],
     [nweibull-ws|portair.local],
     [host_mk=$srcdir/host/$enable_hostname_dotfiles/host.mk],
     [*],
     [AC_MSG_FAILURE(
        [--enable-hostname-dotfiles for $enable_hostname_dotfiles is not supported])])])
AC_CHECK_PROGS([CURL], [curl])
AC_ARG_VAR([CURL], [the cURL command])
AC_CHECK_PROGS([DIFF], [diff])
AC_ARG_VAR([DIFF], [the diff command])
AC_CHECK_PROGS([EMACS], [emacs])
AC_ARG_VAR([EMACS], [the Emacs editor command])
AC_CHECK_PROGS([ICONV], [iconv])
AC_ARG_VAR([ICONV], [the iconv command])
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_CHECK_PROGS([PATCH], [patch])
AC_ARG_VAR([PATCH], [the patch command])
AC_CHECK_PROGS([UNZIP], [unzip])
AC_ARG_VAR([UNZIP], [the unzip command])
AC_PATH_PROGS([ZSHELL], [zsh])
AC_ARG_VAR([ZSHELL], [the zsh command])
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
