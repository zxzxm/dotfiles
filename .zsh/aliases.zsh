#-----------------------------------------------------------------------
# GLOBAL ALIASES
typeset -Ag abbreviations
abbreviations=(
  "za"      "| awk"
  "zl"      "| less"
  "zg"      "| grep"
  "zh"      "| head"
  "zt"      "| tail"
  "zs"      "| sort"
  "zu"      "| sort -u"
  "zw"      "| wc -l"
  "zx"      "| xargs "
  "NE"      "2> /dev/null"
  "NUL"     "> /dev/null 2>&1"
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

if [[ "$OSTYPE" =~ "darwin" ]]; then
  # Get the brew coreutils aliases (GNU stuff)
  #source $(brew --prefix coreutils)/aliases
  GLSPATH=$(brew --prefix coreutils)
  alias ls='$GLSPATH/bin/gls -lhGF --color=auto'
else
  alias ls='ls -lhGF --color=auto'
fi

alias sudo='sudo '
alias vi='vim'
alias view='vim -R '
#alias jh='ssh therma000@jumphost'
alias history='history -i'

alias top=htop

alias sqlite=sqlite3

alias mysql=altsql

# Show aliases in the which output
which='alias | /usr/bin/which --tty-only --read-alias --show-dot --show-tilde'

# Network
alias vpn='sudo netcfg net-vpnc'
alias dvpn='sudo netcfg -d net-vpnc'

alias myip='curl --connect-timeout 3 ipconfig.me'

alias tunnel='sshuttle --D --pidfile=/tmp/sshuttle.pid -r nkosi-remote --dns 0/0'
alias stoptunnel='[[ -f /tmp/sshuttle.pid ]] && kill `cat /tmp/sshuttle.pid`'

if [[ "$OSTYPE" =~ "linux" ]]; then
   # Arch Linux
   alias ss='yaourt -Ss '
   alias inst='yaourt -S'
   alias dl='yaourt -Ql'
   alias up='yaourt -Syu'
   alias remove='yaourt -Rd'
   alias rc='sudo vi /etc/rc.conf'
fi

# Ruby and Gems
alias gs='gem search -r '
alias gi='gem install '

alias sz='source ~/.zshrc'
alias ez='vim ~/.zshrc'

alias b='bundle exec'

alias j=z

#-----------------------------------------------------------------------
# Directory Aliases
hash -d lr=~/Dropbox/src/legal_response/
hash -d dv=~/Dropbox/work/DayView
hash -d aw=~/.config/awesome
hash -d em=~/.emacs.d/personal
hash -d mem=~/Dropbox/src/glenwood/members/members_reboot
hash -d http=/srv/http
hash -d html=/srv/http

