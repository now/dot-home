#! /bin/sh

install-package () {
  port -q installed "$1" | fgrep "  $1" || sudo port -N -q install "$1"
}

for p in autoconf \
	   curl \
	   emacs-app-devel \
	   exif \
	   grep \
	   socat \
	   zsh; do
  install-package $p
done

mkdir -p .build &&
  autoreconf -i &&
  (cd .build && ../configure $@)
