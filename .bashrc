# -*- Mode:  shell-script -*-
# Starting init file (see ~/.bash/ for more)

# Only run this stuff if we're in an interactive session (not SCP)
if  [ ${TERM} != "dumb" ]; then

    export SHELL=/bin/bash

    # source PATH info etc
    [ -f ~/.bash/profile ] && . ~/.bash/profile

    # set up a basic prompt
    PS1='\u.\h (\w) \$ '

    # source global stuff
    [ -f ~/.bash/functions ] && . ~/.bash/functions

    # set up a fancy prompt
    [ -f ~/.bash/prompt ] && . ~/.bash/prompt

    # set up dir_colors
    if [ -f ~/.bash/dir_colors ]; then
	eval `dircolors --sh ~/.bash/dir_colors`
    else
	eval `dircolors`
    fi

    # source aliases
    [ -f ~/.bash/aliases ] && . ~/.bash/aliases

    #do some miscellaneous stuff 
    [ -f ~/.bash/misc ] && . ~/.bash/misc

    #do some site specific stuff (do this last)
    [ -f ~/.bash/local ] && . ~/.bash/local

    # Bash completions
    [ -f /etc/bash_completion ] && . /etc/bash_completion

    TERM=xterm; export TERM

    cd ~
fi
