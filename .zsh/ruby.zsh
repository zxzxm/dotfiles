#-----------------------------------------------------------------------
# Ruby 
#

#export GEM_HOME=$HOME/.gems

# Rbenv (https://github.com/sstephenson/rbenv)
if [[ -s "$HOME/.rbenv" ]]; then
    export PATH="$HOME/.rbenv/bin:$PATH"
    eval "$(rbenv init -)"
fi


# For Rsense (Emacs ruby)
export RSENSE_HOME="~/.emacs.d/vendor/rsense"
