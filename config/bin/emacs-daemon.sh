#!/bin/bash

# Make sure that the environment is bootstrapped before starting the emacs
# daemon.

# TODO: This needs a check if we are on linux and then it should run a
# different thing.

. $HOME/.profile
exec ~/.nix-profile/Applications/Emacs.app/Contents/MacOS/Emacs --daemon
