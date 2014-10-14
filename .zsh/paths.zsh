#-----------------------------------------------------------------
# Paths
cdpath=(. .. ~)

path=(${HOME}/bin ${HOME}/.local/bin ${HOME}/perl5/bin ${HOME}/node_modules/.bin /usr/local/bin $path)

#fpath=($fpath)

# OSX specific paths
if [[ $OSTYPE == 'darwin13.0.0' ]]; then
    PATH=$PATH:${HOME}/Library/Python/2.7/bin
fi

# Linux specific paths
if [[ $OSTYPE == 'linux-gnu' ]]; then
    PATH=$PATH:/usr/bin/core_perl
    PATH=$PATH:/usr/bin/vendor_perl
fi

# Remove duplicates from path
typeset -U path cdpath fpath manpath
