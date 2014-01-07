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

alias vi='vim'
alias view='vim -R '

# History
alias history='history -i'
alias h='history -i'
alias hgrep="fc -El 0 | grep"

# Other
alias top=htop
alias dmesg='dmesg -L'
alias sqlite=sqlite3
alias mysql=altsql
alias msgs='journalctl -f'

# Show aliases in the which output
which='alias | /usr/bin/which --tty-only --read-alias --show-dot --show-tilde'

# Network
alias vpn='sudo netcfg net-vpnc'
alias dvpn='sudo netcfg -d net-vpnc'

alias myip="curl --silent checkip.dyndns.org | grep --extended-regexp --only-matching '[0-9\.]+'"

alias tunnel='sshuttle --D --pidfile=/tmp/sshuttle.pid -r nkosi-remote --dns 0/0'
alias stoptunnel='[[ -f /tmp/sshuttle.pid ]] && kill `cat /tmp/sshuttle.pid`'

alias dig='dig +short '

if [[ "$OSTYPE" =~ "linux" ]]; then
   # Arch Linux
   alias ss='yaourt -Ss '
   alias inst='yaourt -S'
   alias dl='yaourt -Ql'
   alias up='yaourt -Syu'
   alias remove='yaourt -Rd'
   alias rc='sudo vi /etc/rc.conf'

   #
   alias -s pl='emacsclient -n'
   alias -s rb='emacsclient -n'
   alias -s pdf='evince'
   alias -s ps='evince'
fi

# LDAP
alias lsc='ldapsearch -LLL -h drc-pot-001 -D cn=root -w secret '

# Ruby and Gems
alias gs='gem search -r '
alias gi='gem install '

alias sz='source ~/.zshrc'
alias ez='vim ~/.zshrc'

alias b='bundle exec'

alias j=z

# Quick way to rebuild the Launch Services database and get rid
# # of duplicates in the Open With submenu.
[[ $OSTYPE =~ 'darwin' ]] && alias fixopenwith='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'

#-----------------------------------------------------------------------
# Directory Aliases
hash -d lr=~/Dropbox/src/legal_response/
hash -d dv=~/Dropbox/work/DayView
hash -d aw=~/.config/awesome
hash -d em=~/.emacs.d/personal
hash -d mem=~/Dropbox/src/glenwood/members/members_reboot
hash -d http=/srv/http
hash -d html=/srv/http

#-----------------------------------------------------------------------
# Open with
alias -s html=firefox
