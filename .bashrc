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


######################
#### Autocomplete ####
######################

if ! shopt -oq posix; then
   if [ -f /usr/share/bash-completion/bash_completion ]; then
     . /usr/share/bash-completion/bash_completion
   elif [ -f /etc/bash_completion ]; then
     . /etc/bash_completion
   fi
fi


################
#### Colors ####
################

export TERM=xterm-256color


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
