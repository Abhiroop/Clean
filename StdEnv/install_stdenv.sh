
set -e

INSTALL_STDENV_DIR="/home/johnvg/build/test/clean/lib/stdenv"
CLEANLIB="/home/johnvg/build/test/clean/exe"
CLEANPATH="/home/johnvg/build/test/clean/stdenv"
CLMP="/home/johnvg/build/test/clean/bin/clm"
CLMFLAGS="-nw"

export INSTALL_STDENV_DIR
export CLEANLIB
export CLEANPATH
export CLMP
export CLMFLAGS

./install.sh
