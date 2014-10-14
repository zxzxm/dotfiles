#----------------------------------------------------------------
# OSX
if [[ $OSTYPE =~ 'darwin' ]]; then
    export BREWPREFIX=`brew --prefix`

    # Colorize with GRC
    source "`brew --prefix grc`/etc/grc.bashrc"

fi
