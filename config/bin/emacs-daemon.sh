#!/bin/bash

# Make sure that the environment is bootstrapped before starting the emacs
# daemon.

. $HOME/.profile
exec ~/.nix-profile/Applications/Emacs.app/Contents/MacOS/Emacs --daemon
