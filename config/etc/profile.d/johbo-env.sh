
NIX_LINK=$HOME/.nix-profile


if [ -e $NIX_LINK/Applications/Emacs.app/Contents/MacOS/Emacs ]
then
    export ALTERNATE_EDITOR=""
    export EDITOR='emacsclient -t'
    export VISUAL="emacsclient -a $NIX_LINK/Applications/Emacs.app/Contents/MacOS/Emacs"
fi
