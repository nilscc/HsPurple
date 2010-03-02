HsPurple - Haskell bindings to libpurple
----------------------------------------

This project tries to write Haskell bindings for the IM library libpurple.
Libpurple is used by Pidgin, Finch and a lot of other open source programms.
Take a look at the [developers.pidgin.im][http://developer.pidgin.im/] for more
informations.

## Building HsPurple

To build HsPurple you need the following command:

    cabal configure
    cabal build --hsc2hs-options="`pkg-config --cflags purple`"

You can also use the ./run.sh which runs exactly that command.
