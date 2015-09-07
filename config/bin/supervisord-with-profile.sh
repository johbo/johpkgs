#!/bin/bash

# Make sure that the environment is bootstrapped before starting supervisord


# TODO: Find a better way to make sure that a useful path is provided.
echo "Sourcing /etc/.profile"
. /etc/profile
echo "Sourcing ~/.profile"
. $HOME/.profile

echo "Launching supervisord"
exec ~/.nix-profile/bin/supervisord $@
