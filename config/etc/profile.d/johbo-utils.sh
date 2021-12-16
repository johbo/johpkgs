
#
# Common alias and function definitions
#


# If not running interactively, don't do anything
[ -z "$PS1" ] && return

if [ -n "$ZSH_VERSION" ]
then
    _export_parameter=""
else
    _export_parameter="-f"
fi


# Nix
function nix_build_linux() {
    nix-build \
        --arg pkgs '(import <nixpkgs> { system="x86_64-linux"; })' \
        $@
}

export $_export_parameter nix_build_linux



# Emacs shortcuts
function emacs() {
    $VISUAL -c $@
}

function ee() {
    $EDITOR $@
}

function eo() {
    $VISUAL -c -n $@
}

export $_export_parameter emacs ee eo



# mercurial version control
function hgcl() {
    commit_id=${1:-.}
    hg log -r "reverse(ancestors($commit_id))" -G
}

function hgud() {
    hg up -r `hg id -r default -i upstream`
}

function hgus() {
    hg up -r `hg id -r stable -i upstream`
}

export $_export_parameter hgcl hgud hgus


# git version control
alias gai='git add -i'
function gbl {
    git branch --list \*${1}\*
}
alias gci='git commit'
alias gca='git commit -a'
alias gdi='git diff'
alias gpr='git pull --rebase'

function gst() {
    git status
}

alias gfu='git fetch -v upstream; git fetch -v origin'

alias gserve='echo "on port 9418"; git daemon --reuseaddr --base-path=. --export-all --verbose'

export $_export_parameter gbl gst



# Python development
alias rm-pyc='find . -name "*.pyc" -delete'
alias pygrep='grep --color=always --include="*.py" -rn'



# Useful tricks
alias pp='python -mjson.tool'
alias watchman='watchman --statefile=$HOME/var/watchman.state'
