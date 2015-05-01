
NIX_LINK=$HOME/.nix-profile



# If not running interactively, don't do anything
[ -z "$PS1" ] && return


# Hook up editor configuration for emacs on darwin
# Expects the emacs app to be available in ~/.nix-profile
if [ -e $NIX_LINK/Applications/Emacs.app/Contents/MacOS/Emacs ]
then
    export ALTERNATE_EDITOR=""
    export EDITOR='emacsclient -t'
    export VISUAL="emacsclient -a $NIX_LINK/Applications/Emacs.app/Contents/MacOS/Emacs"
fi



# Prompt tweaks

# Git repositories
PS1='\n\[\e[0;34m\]\u@\h: \e[0;36m\w \[\e[0;33m\]$(__git_ps1 "%s")\[\e[0m\]\n$ '
