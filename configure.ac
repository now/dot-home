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
AC_PROG_INSTALL
AC_PROG_MKDIR_P
AC_CONFIG_FILES([Makefile])
AC_OUTPUT
