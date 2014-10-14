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

export EDITOR=vim
export VISUAL=vim
export ALTERNATE_EDITOR=emacs

export BROWSER=firefox

#-----------------------------------------------------------------------
# Quicksilver-like thingy for zsh
#source $ZSH_CUSTOM/plugins/zaw/zaw.zsh
bindkey '^xr' zaw-history
bindkey '^xp' zaw-process
bindkey '^xg' zaw-git-files
bindkey '^xs' zaw-ssh-hosts
bindkey '^xa' zaw-applications
