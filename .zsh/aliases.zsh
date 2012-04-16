#-----------------------------------------------------------------------
# GLOBAL ALIASES
typeset -Ag abbreviations
abbreviations=(
  "Ia"      "| awk"
  "Im"      "| most"
  "Il"      "| less"
  "Ig"      "| grep"
  "Ip"      "| $PAGER"
  "Ih"      "| head"
  "It"      "| tail"
  "Is"      "| sort"
  "Iu"      "| sort -u"
  "Iv"      "| ${VISUAL:-${EDITOR}}"
  "Iw"      "| wc -l"
  "Ix"      "| xargs "
  "NE"      "2> /dev/null"
  "NUL"     "> /dev/null 2>&1"
  "sprunge" "| curl -F 'sprunge=<-' http://sprunge.us"
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

if [[ "$OSTYPE" == "darwin11.0" ]]; then
  # Get the brew coreutils aliases (GNU stuff)
#  source /usr/local/Cellar/coreutils/8.12/aliases
#  alias ls='gls -lhGF --color=auto'
else
  alias ls='ls -lhGF --color=auto'
fi

alias sudo='sudo '
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
alias remove='yaourt -Rd'
alias rc='sudo vi /etc/rc.conf'

# Ruby and Gems
alias gs='gem search -r '
alias gi='gem install '

alias sz='source ~/.zshrc'
alias ez='vim ~/.zshrc'

alias b='bundle exec'

alias j=z

#-----------------------------------------------------------------------
# Directory Aliases
hash -d lr=/Users/thermans/Dropbox/src/legal_response/
hash -d aw=/Users/thermans/.config/awesome
hash -d em=/Users/thermans/.emacs.d/personal
hash -d mem=/Users/thermans/Dropbox/src/glenwood/members/members_new
hash -d http=/srv/http
hash -d html=/srv/http
