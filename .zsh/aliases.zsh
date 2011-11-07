#-----------------------------------------------------------------------
# GLOBAL ALIASES
typeset -Ag abbreviations
abbreviations=(
  "Ia"    "| awk"
  "Im"    "| most"
  "Il"    "| less"
  "Ig"    "| grep"
  "Ip"    "| $PAGER"
  "Ih"    "| head"
  "It"    "| tail"
  "Is"    "| sort"
  "Iu"    "| sort -u"
  "Iv"    "| ${VISUAL:-${EDITOR}}"
  "Iw"    "| wc -l"
  "Ix"    "| xargs "
)

magic-abbrev-expand() {
    local MATCH
    LBUFFER=${LBUFFER%%(#m)[_a-zA-Z0-9]#}
    LBUFFER+=${abbreviations[$MATCH]:-$MATCH}
    zle self-insert
}

no-magic-abbrev-expand() {
  LBUFFER+=' '
}

zle -N magic-abbrev-expand
zle -N no-magic-abbrev-expand
bindkey " " magic-abbrev-expand
bindkey "^x " no-magic-abbrev-expand

#-----------------------------------------------------------------------
# ALIASES
alias sudo='sudo '
alias ls='ls -lhGF --color=auto'
alias vi='vim'
alias view='vim -R '
alias jh='ssh therma000@jumphost'
alias history='history -i'

alias top=htop

alias sqlite=sqlite3

# Network

alias vpn='sudo netcfg net-vpnc'
alias dvpn='sudo netcfg -d net-vpnc'

# Arch Linux
alias ss='yaourt -Ss '
alias inst='yaourt -S'
alias dl='yaourt -Ql'
alias up='yaourt -Syu'

# Ruby and Gems
alias gs='gem search -r '
alias gi='gem install '

alias sz='source ~/.zshrc'
alias ez='vim ~/.zshrc'

alias b='bundle exec'

alias j=z
