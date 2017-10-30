
NIX_LINK=$HOME/.nix-profile



# If not running interactively, don't do anything
[ -z "$PS1" ] && return


# Hook up editor configuration for emacs on darwin
# Expects the emacs app to be available in ~/.nix-profile
if [ -e $NIX_LINK/Applications/Emacs.app/Contents/MacOS/Emacs ]
then
    foundEmacs=$NIX_LINK/Applications/Emacs.app/Contents/MacOS/Emacs
elif [ -e $NIX_LINK/bin/emacs ]
then
    foundEmacs=$NIX_LINK/bin/emacs
fi


export NIX_PATH=ssh-config-file=$HOME/etc/ssh/nix-default-conf:$NIX_PATH

# Remote building support
export NIX_BUILD_HOOK=${NIX_LINK}/libexec/nix/build-remote.pl
export NIX_REMOTE_SYSTEMS=${HOME}/etc/nix/remote-systems.conf
export NIX_CURRENT_LOAD=${HOME}/tmp/nix-load


if [ -n "$foundEmacs" ]
then
    export ALTERNATE_EDITOR=""
    export EDITOR='emacsclient -t'
    export VISUAL="emacsclient -a $foundEmacs"
fi


# Language settings, partially important for Python on Darwin
export LC_ALL=en_US.UTF-8
export LANG=en_US.UTF-8


# Postgres development setup
export PGDATA=$HOME/var/lib/postgresql/9.6


# Prompt tweaks

# Git repositories
PS1='\n\[\e[0;34m\]\u@\h: \e[0;36m\w \[\e[0;33m\]$(__git_ps1 "%s")\[\e[0m\]\n$ '
