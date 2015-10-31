JSON
====
Haskell JSON library, implemented for fun. It follows the latest standard set
by [JSON](https://json.org) where root object is allowed to be any JSON value
and not just a object or array.

This library is just started, it lacks many basic features.

How-to
------
Follow instructions on how to install
[Stack](https://www.haskell.org/downloads).

To build the library, execute from root directory:

    $ stack build

To build and install the command-line interface, execute from root directory:

    $ stack install
    $ json <foobar.json>

The `stack install` command is exactly the same as `stack build` with the only
difference that stack moves the binary to `~/.local/bin`. So in order to
execute the command-line interface you must have this directory in your `PATH`.
