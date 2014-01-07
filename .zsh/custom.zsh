#-----------------------------------------------------------------------
# My ZSH Customizations

#-----------------------------------------------------------------------
# AUTOLOADS
#
# "Execute" a file, and have it open default program.  i.e. "% ./foo.pdf"
# opens PDF viewer
autoload -U zsh-mime-setup

# Rename many files
# See here for details: http://strcat.de/zsh/#zmv
autoload -U zmv

#-----------------------------------------------------------------------
# ENVIRONMENT

cdpath=(. .. ~)

PATH=${HOME}/bin
PATH=$PATH:${HOME}/.cabal/bin
PATH=$PATH:${HOME}/perl5/bin
PATH=$PATH:/usr/local/share/npm/bin
PATH=$PATH:/usr/local/bin
PATH=$PATH:/usr/bin
PATH=$PATH:/bin
PATH=$PATH:/usr/sbin
PATH=$PATH:/sbin
PATH=$PATH:/usr/bin/core_perl
PATH=$PATH:/usr/bin/vendor_perl
PATH=$PATH:/usr/local/sbin
PATH=$PATH:${HOME}/node_modules/.bin

fpath=(/usr/local/share/zsh-completions $fpath)


# Remove duplicates from path
typeset -U path cdpath fpath manpath

export EDITOR=vim
export VISUAL=vim
export ALTERNATE_EDITOR=emacs

export BROWSER=firefox

# Perl Stuff
# Use home folder for modules
eval $(perl -I$HOME/perl5/lib/perl5 -Mlocal::lib)

#-----------------------------------------------------------------------
# HISTORY
HISTFILE=~/.zsh-history

# Do not put "history" commands in the history, duh
setopt hist_no_store

#-----------------------------------------------------------------------
# OPTIONS
#

# Don't push the same dir twice.
setopt pushd_ignore_dups

# Don't match dotfiles. ever.
setopt noglobdots

# Use zsh style word splitting
setopt noshwordsplit

# If command cannot be executed and name is a directory, cd to it
setopt auto_cd

# Use #, ~, and ^ for filename generation
setopt extended_glob

# Disable CTRL-S
setopt no_flow_control

# beeps are annoying
setopt no_beep

# Keep echo "file" > file from clobbering file
#setopt NO_CLOBBER

# Case insensitive globbing
setopt no_case_glob

# Be Reasonable!
setopt numeric_glob_sort

# I don't know why I never set this before.
setopt extended_glob

# hows about arrays be awesome?  (that is, frew${cool}frew has frew surrounding all the variables, not just first and last
setopt rc_expand_param

# Use Emacs keys
bindkey -e

# No username completion
setopt no_cdable_vars

#-----------------------------------------------------------------------
# FUNCTIONS

# press esc-e for editing command line in $EDITOR or $VISUAL
if autoload edit-command-line && zle -N edit-command-line ; then
    #k# Edit the current line in \kbd{\$EDITOR}
    bindkey '^xe' edit-command-line
fi

# Press "ctrl-e d" to insert the actual date in the form yyyy-mm-dd
_bkdate() { BUFFER="$BUFFER$(date '+%F')"; CURSOR=$#BUFFER; }
zle -N _bkdate
bindkey '^ed' _bkdate

# add a command line to the shells history without executing it
commit-to-history() {
    print -s ${(z)BUFFER}
    zle send-break
}
zle -N commit-to-history
bindkey "^x^h" commit-to-history

# only slash should be considered as a word separator:
slash-backward-kill-word() {
    local WORDCHARS="${WORDCHARS:s@/@}"
    # zle backward-word
    zle backward-kill-word
}
zle -N slash-backward-kill-word

# Kill everything in a word up to its last "/"
bindkey '^xv' slash-backward-kill-word

# press esc-m for inserting last typed word again (thanks to caphuso!)
insert-last-typed-word() { zle insert-last-word -- 0 -1 };
zle -N insert-last-typed-word

# Insert last typed word
bindkey "^xm" insert-last-typed-word

# run command line as user root via sudo:
sudo-command-line() {
    [[ -z $BUFFER ]] && zle up-history
    [[ $BUFFER != sudo\ * ]] && BUFFER="sudo $BUFFER"
    zle end-of-line
}
zle -N sudo-command-line

#k# Put the current command line into a \kbd{sudo} call
bindkey "^x^x" sudo-command-line

# Wrap the command line in a for statement

make-for-loop() {
    [[ -z $BUFFER ]] && zle up-history
    [[ $BUFFER != for\ * ]] && BUFFER="for x in (); do $BUFFER; done"
}
zle -N make-for-loop

bindkey "^xf" make-for-loop

### jump behind the first word on the cmdline.
### useful to add options.
function jump_after_first_word() {
    local words
    words=(${(z)BUFFER})

    if (( ${#words} <= 1 )) ; then
        CURSOR=${#BUFFER}
    else
        CURSOR=${#${words[1]}}
    fi
}
zle -N jump_after_first_word

bindkey '^x1' jump_after_first_word

# use the new *-pattern-* widgets for incremental history search
bindkey '^r' history-incremental-pattern-search-backward
bindkey '^s' history-incremental-pattern-search-forward

### mkdir && cd
function mcd()
{
	test -z "$1" && echo mcd: no path given && return
	test -d "$1" && print "mcd: Directory $1 already exists"
	mkdir -p -- "$1"
	cd -- "$1"
}


#-----------------------------------------------------------------------
# MISC

# Colorize STDERR
#exec 2>>(while read line; do print '\e[91m'${(q)line}'\e[0m' > /dev/tty; print -n $'\0'; done &)

# Haste pastebin
export HASTE_SERVER=http://pastebin.cable.comcast.com

# Helper for less
#LESSOPEN="|lesspipe.sh %s"; export LESSOPEN
export LESSOPEN="| /usr/bin/src-hilite-lesspipe.sh %s"
export LESS=' -R '

# For Java apps
#export _JAVA_AWT_WM_NONREPARENTING=1


# Simple calculator
calc () {
    test -z "$1" && echo "usage example: calc \"3*6\"" && return
    #awk "BEGIN{ print $* }"
    echo $(($*))
}

#-----------------------------------------------------------------------
# Ruby setup
#

#export GEM_HOME=$HOME/.gems

# Rbenv (https://github.com/sstephenson/rbenv)
if [[ -s "$HOME/.rbenv" ]]; then
    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"
fi

# For zsh-syntax-highlighting
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)

#-----------------------------------------------------------------------
# Quicksilver-like thingy for zsh
source $ZSH_CUSTOM/plugins/zaw/zaw.zsh
bindkey '^xr' zaw-history
bindkey '^xp' zaw-perldoc
bindkey '^xg' zaw-git-files
#bindkey '^xd' zaw-cdr

# OS specific settings ------------------------------------------------
# Linux

if [[ $OSTYPE == 'linux-gnu' ]]; then

    # For MPD clients
    export MPD_HOST=/home/thermans/.mpd/socket

    # Colorize with GRC
    . "$HOME/.zsh/grc.zsh"

    # Proxy
    #export http_proxy="http://therma000:IzzyZ0ra!@10.21.151.203:8080"
    export http_proxy="http://127.0.0.1:10000"
    export https_proxy="http://127.0.0.1:10000"

    # THEME
    export ZSH_THEME="tim-zenburn"

fi

# OSX
if [[ $OSTYPE =~ 'darwin' ]]; then
    export BREWPREFIX=`brew --prefix`

    # Colorize with GRC
    source "`brew --prefix grc`/etc/grc.bashrc"

    # For powerline
    PATH=$PATH:${HOME}/Library/Python/2.7/bin

    # THEME
    export ZSH_THEME="tim-zenburn"

fi

# Set the theme
source ${ZSH_CUSTOM}/themes/${ZSH_THEME}.zsh-theme

# For Rsense (Emacs ruby)
export RSENSE_HOME="~/.emacs.d/vendor/rsense"


# For "weatherme" https://github.com/shapeshed/weatherme
export KEY="0a076ac73ee032925db493cbaffad41e"
export LATLON="39.018500166927,-77.05239772796631"

# Amazon Web Services
#export AWS_ACCESS_KEY_ID=0JMB4AXJ18YWCBJ2C9G2
export AWS_ACCESS_KEY_ID=AKIAJHK2CKJHKPJ7DM7A
#export AWS_SECRET_ACCESS_KEY=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
export AWS_SECRET_ACCESS_KEY=ShNat478N7q5EpAy7SsljeWc3MNV8D68Ll0OSZAX
