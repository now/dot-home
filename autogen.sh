#! /bin/sh

out=`xcode-select --install 2>&1` || \
  { echo "$out" | fgrep 'already installed' >/dev/null || exit 1; }

# sudo xcodebuild -license

installpackage () {
  port -q installed "$1" | fgrep "  $1" || sudo port -N -q install "$1"
}

for p in autoconf \
	   emacs-app-devel \
	   exif \
           gmake \
	   grep; do
  installpackage $p
done

mkdir -p .build &&
  autoreconf -i &&
  (cd .build && ../configure $@)
