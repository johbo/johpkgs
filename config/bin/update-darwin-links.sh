#!/bin/bash

# Symlink darwin specific things so that they are found by launchd

NIX_LINK=$HOME/.nix-profile
AGENTS_SRC=$NIX_LINK/Library/LaunchAgents
AGENTS=$HOME/Library/LaunchAgents


mkdir -p $AGENTS

for file in `ls $AGENTS_SRC/*.plist`
do
    target=$AGENTS/`basename $file`

    if [ -e $target ]
    then
        echo "'$target' already existing, skipping."
        continue
    fi

    echo "'$target' missing, adding."
    ln -s $file $target
done
