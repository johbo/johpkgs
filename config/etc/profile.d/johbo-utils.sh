
#
# Common alias and function definitions
#


# If not running interactively, don't do anything
[ -z "$PS1" ] && return



# Emacs shortcuts
alias emacs='$VISUAL -c'
alias ee='$EDITOR'
alias eo='$VISUAL -n'



# mercurial version control
alias hgcl='hg log -r "reverse(ancestors(.))" -G'
alias hgud='hg up -r `hg id -r default -i upstream`'
alias hgus='hg up -r `hg id -r stable -i upstream`'


# git version control
alias gai='git add -i'
function gbl {
    git branch --list \*${1}\*
}
alias gci='git commit'
alias gca='git commit -a'
alias gdi='git diff'
alias gpr='git pull --rebase'
alias gst='git status'

alias gfu='git fetch -v upstream; git fetch -v origin'

alias gserve='echo "on port 9418"; git daemon --reuseaddr --base-path=. --export-all --verbose'



# Python development
alias rm-pyc='find . -name "*.pyc" -delete'
alias pygrep='grep --color=always --include="*.py" -rn'



# Useful tricks
alias pp='python -mjson.tool'
alias watchman='watchman --statefile=$HOME/var/watchman.state'
