# Debug
#setopt XTRACE VERBOSE
#
# Path to your oh-my-zsh configuration.
export ZSH=$HOME/.oh-my-zsh

# Don't mess with my title bar
#export DISABLE_AUTO_TITLE=true

# Set to the name theme to load.
# Look in ~/.oh-my-zsh/themes/
#export ZSH_THEME="robbyrussell"
export ZSH_THEME="tim"

# Set to this to use case-sensitive completion
# export CASE_SENSITIVE="true"

# Comment this out to disable weekly auto-update checks
export DISABLE_AUTO_UPDATE="true"

# Uncomment following line if you want to disable colors in ls
# export DISABLE_LS_COLORS="true"
#
# Uncomment following line if you want red dots to be displayed while waiting for completion
export COMPLETION_WAITING_DOTS="true"

# Which plugins would you like to load? (plugins can be found in ~/.oh-my-zsh/plugins/*)
# Example format: plugins=(rails git textmate ruby lighthouse)
plugins=(
  archlinux
  brew
  bundler
  cpanm
  extract
  git
  github
  gnu-utils
  history-substring-search
  node
  npm
  pow
  powder
  rails
  rails3
  ruby
  rvm
  svn
  zaw
  )

export ZSH_CUSTOM=$HOME/.zsh

source $ZSH/oh-my-zsh.sh

#--------------------------------------------------------------------------
# These are my customizations.  Didn't put them in the
# ".oh-my-zsh/custom" directory because I want to be able to update
# that from git without disturbing anything


#export MY_ZSH=$HOME/.zsh
#source $ZSH_CUSTOM/custom.zsh
#source $ZSH_CUSTOM/aliases.zsh

#myplugins=(
#  zaw
#  zsh-syntax-highlighting-filetypes
#)

#for myplugin ($myplugins); do
#  if [ -f $MY_ZSH/plugins/$myplugin/$myplugin.zsh ]; then
#    source $MY_ZSH/plugins/$myplugin/$myplugin.zsh
#  fi
#done

# My Theme
if [[ $TERM == "eterm-color" ]]
then
    source $ZSH_CUSTOM/themes/emacs.zsh-theme
else
    source $ZSH_CUSTOM/themes/$ZSH_THEME.zsh-theme
fi


PATH=$PATH:$HOME/.rvm/bin # Add RVM to PATH for scripting
