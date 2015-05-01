
#
# Common alias and function definitions
#

# TODO: find better way to handle this
alias emacs='$VISUAL -c'
alias ee='$EDITOR'
alias eo='$VISUAL -n'

# mercurial version control

# git version control
alias gai='git add -i'
function gbl {
    git branch --list \*${1}\*
}
alias gci='git commit'
alias gca='git commit -a'
alias gdi='git diff'
alias gst='git status'

alias gfu='git fetch -v upstream; git fetch -v origin'

alias gserve='echo "on port 9418"; git daemon --reuseaddr --base-path=. --export-all --verbose'

# Python development
alias rm-pyc='find . -name "*.pyc" -delete'


# Useful tricks
alias pp='python -mjson.tool'
