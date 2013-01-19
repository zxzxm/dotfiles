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
# path=(
#     $PATH
#     /home/thermans/.gem/bin
#     /opt/java/jre/bin
#     /opt/oracle
#     /home/thermans/bin
#     /home/thermans/.ec2/bin
#     /home/thermans/.seed/bin
#     /usr/share/java/apache-ant/bin
# )

PATH=${HOME}/bin
PATH=$PATH:${HOME}/node_modules/.bin
PATH=$PATH:/usr/local/bin
PATH=$PATH:/usr/bin
PATH=$PATH:/bin
PATH=$PATH:/usr/sbin
PATH=$PATH:/sbin
PATH=$PATH:/usr/bin/core_perl
PATH=$PATH:/usr/bin/vendor_perl
PATH=$PATH:/usr/local/sbin

if [[ $OSTYPE =~ 'darwin' ]]; then
  PATH=/usr/local/Cellar/ccache/3.1.8/libexec:$PATH # ccache
fi

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

# Autojump (http://wiki.github.com/joelthelion/autojump/)
#source /etc/profile
#source /etc/profile.d/autojump.zsh

#-----------------------------------------------------------------------
# THEME
[[ $OSTYPE =~ 'darwin' ]] && ZSH_THEME="tim-zenburn"
[[ $OSTYPE == 'linux-gnu' ]] && ZSH_THEME="tim-zenburn"
export ZSH_THEME

source ${ZSH_CUSTOM}/themes/${ZSH_THEME}.zsh-theme

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

#  From: http://bmaland.com/2009/04/07/zsh-tip-of-the-day%3A-%232.html
# Prefix the previous line with "sudo " then execute it
rerun-with-sudo () {
  LBUFFER="sudo !!"
  zle accept-line
}
zle -N rerun-with-sudo

bindkey '\C-x\C-x' rerun-with-sudo

# press esc-e for editing command line in $EDITOR or $VISUAL
if autoload edit-command-line && zle -N edit-command-line ; then
    #k# Edit the current line in \kbd{\$EDITOR}
    bindkey '^xe' edit-command-line
fi

# Press "ctrl-e d" to insert the actual date in the form yyyy-mm-dd
_bkdate() { BUFFER="$BUFFER$(date '+%F')"; CURSOR=$#BUFFER; }
zle -N _bkdate
bindkey '^xd' _bkdate

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
}
zle -N sudo-command-line

#k# Put the current command line into a \kbd{sudo} call
bindkey "^xs" sudo-command-line

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
LESSOPEN="|lesspipe %s"; export LESSOPEN

# For Java apps
export _JAVA_AWT_WM_NONREPARENTING=1

# List all installed packages
function pkgs () {
    # yaourt -Qei $(yaourt -Qu|cut -d" " -f 1)|awk ' BEGIN {FS=":"}/^Name/{printf("\033[1;34m%s\033[0;37m", $2)}/^Description/{print $2}'
    yaourt -Qei $(yaourt -Qu|cut -d" " -f 1)|awk ' BEGIN {FS=":"}/^Name/{printf("%s", $2)}/^Description/{print $2}'
}

# Show details of an package
function pkg () {
    test -z "$1" && echo "usage: pkg packagename" && return
    yaourt -Qi $1
}

# Simple calculator
calc () {
    test -z "$1" && echo "usage example: calc \"3*6\"" && return
    awk "BEGIN{ print $* }"
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

# RVM (http://beginrescueend.com/)
unsetopt auto_name_dirs
[[ -s "$HOME/.rvm/scripts/rvm" ]] && unsetopt auto_name_dirs
[[ -s "$HOME/.rvm/scripts/rvm" ]] && . "$HOME/.rvm/scripts/rvm"

# For zsh-syntax-highlighting
ZSH_HIGHLIGHT_HIGHLIGHTERS=(main brackets pattern)

#-----------------------------------------------------------------------
# Use GRC to colorize
[[ $OSTYPE =~ 'darwin' ]] && source "`brew --prefix grc`/etc/grc.bashrc"
[[ $OSTYPE == 'linux-gnu' ]] && . "$HOME/.zsh/grc.zsh"

#-----------------------------------------------------------------------
# Quicksilver-like thingy for zsh
source $ZSH_CUSTOM/plugins/zaw/zaw.zsh
bindkey '^xr' zaw-history
bindkey '^xp' zaw-perldoc
bindkey '^xg' zaw-git-files
bindkey '^xd' zaw-cdr

[[ $OSTYPE == 'linux-gnu' ]] && xmodmap ~/.xmodmap

[[ $OSTYPE =~ 'darwin' ]] && BREWPREFIX=`brew --prefix`

# Z
. $BREWPREFIX/etc/profile.d/z.sh

# For Rsense (Emacs ruby)
export RSENSE_HOME="~/.emacs.d/vendor/rsense"
