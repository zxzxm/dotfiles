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

# These are the base plugins that are not host specific
plugins=(
  cpanm
  colored-man
  dirpersist
  extract
  fabric
  history-substring-search
  gnu-utils
  last-working-dir
  mercurial
  node
  npm
  perl
  pip
  rails
  rake
  rsync
  svn
  systemadmin
  web-search
  zaw
  zsh-syntax-highlighting
  z
)

# Mac plugins
if [[ $OSTYPE =~ "darwin" ]]; then
    plugins+=(
        osx
        brew
        pow
        powder
        powify
    )
fi

# Linux plugins
if [[ $OSTYPE =~ "linux" ]]; then
    plugins+=(
        archlinux
        cabal
        command-not-found
        dircycle
        systemd
        wd
    )
fi

export ZSH_CUSTOM=$HOME/.zsh

source $ZSH/oh-my-zsh.sh

#--------------------------------------------------------------------------
# These are my customizations.  Didn't put them in the
# ".oh-my-zsh/custom" directory because I want to be able to update
# that from git without disturbing anything

source $ZSH_CUSTOM/custom.zsh
source $ZSH_CUSTOM/aliases.zsh

#[[ $OSTYPE == 'darwin11.4.0' ]] && PATH=/usr/local/Cellar/ccache/3.1.9/libexec:$PATH # ccache
