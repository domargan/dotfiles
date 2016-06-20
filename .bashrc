##########################
#### History settings ####
##########################

# Don't put duplicate lines or lines starting with space in the history
HISTCONTROL=ignoreboth

# Append to the history file, don't overwrite it
shopt -s histappend

# History length
HISTSIZE=1000
HISTFILESIZE=2000


#################
#### Aliases ####
#################

if [ -x /usr/bin/dircolors ]; then
    test -r ~/.dircolors && eval "$(dircolors -b ~/.dircolors)" || eval "$(dircolors -b)"
    alias ls='ls --color=auto'

    alias grep='grep --color=auto'
    alias fgrep='fgrep --color=auto'
    alias egrep='egrep --color=auto'
fi

alias emacs='TERM=xterm-256color emacs -nw'
alias pandoc='pandoc --latex-engine=xelatex'
alias cdgo='cd $GOPATH'
