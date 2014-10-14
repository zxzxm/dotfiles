#-----------------------------------------------------------------------
# HISTORY
HISTFILE=~/.zsh-history

# Do not put "history" commands in the history, duh
setopt hist_no_store

# add a command line to the shells history without executing it
commit-to-history() {
    print -s ${(z)BUFFER}
    zle send-break
}
zle -N commit-to-history
bindkey "^x^h" commit-to-history

# use the new *-pattern-* widgets for incremental history search
bindkey '^r' history-incremental-pattern-search-backward
bindkey '^s' history-incremental-pattern-search-forward

