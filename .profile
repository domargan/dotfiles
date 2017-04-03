#################
#### General ####
#################

# If running bash source .bashrc
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	. "$HOME/.bashrc"
    fi
fi


#####################
#### Environment ####
#####################

# Set PATH so it includes private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

# Editors
export EDITOR="emacs"

# Golang environment
export PATH=$PATH:/usr/local/go/bin
export GOPATH=$HOME/Workspace/Code/go
export GOBIN=$GOPATH/bin
export PATH=$PATH:$GOPATH/bin
export PATH=$PATH:/opt/Gogland-171.3780.106/bin
