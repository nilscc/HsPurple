#!/bin/sh
cabal configure && cabal build --hsc2hs-options="`pkg-config --cflags purple`"
