#-----------------------------------------------------------------------
# GLOBAL ALIASES
typeset -Ag abbreviations
abbreviations=(
  "za"      "| awk"
  "zl"      "| less"
  "zg"      "| grep"
  "zh"      "| head"
  "zj"      "| jq '.'"
  "zt"      "| tail"
  "zf"      "tail -f"
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

alias vi='vim'
alias view='vim -R '

alias jh='ssh therma000@jumphost'

# History
alias history='history -i'
alias h='history -i'
alias history-stat="history 0 | awk '{print \$4}' | sort | uniq -c | sort -n -r | head"

# Other
alias top=htop
alias sqlite=sqlite3
#alias mysql=altsql

# Show aliases in the which output
which='alias | /usr/bin/which --tty-only --read-alias --show-dot --show-tilde'

alias myip="curl --silent checkip.dyndns.org | grep --extended-regexp --only-matching '[0-9\.]+'"

alias tunnel='sshuttle --D --pidfile=/tmp/sshuttle.pid -r nkosi-remote --dns 0/0'
alias stoptunnel='[[ -f /tmp/sshuttle.pid ]] && kill `cat /tmp/sshuttle.pid`'

alias dig='dig +short '

# Linux specific aliases -------------------------------
if [[ "$OSTYPE" =~ "linux" ]]; then

    alias ls='ls -lhGF --color=auto'
   # Arch Linux
   alias ss='yaourt -Ss '
   alias inst='yaourt -S'
   alias dl='yaourt -Ql'
   alias up='yaourt -Syu'
   alias remove='yaourt -Rd'
   alias dmesg='dmesg -L'
   alias msgs='journalctl -f'
   alias restart='sudo systemctl restart '
   alias stop='sudo systemctl stop '
   alias start='sudo systemctl start '
   alias status='sudo systemctl status '

   # Network
   alias vpn='sudo vpnc'
   alias dvpn='sudo vpnc-disconnect'

   # Open with
   alias -s pl='emacsclient -n'
   alias -s rb='emacsclient -n'
   alias -s pdf='evince'
   alias -s ps='evince'
fi

# OSX specific aliases ----------------------------------
if [[ "$OSTYPE" =~ "darwin" ]]; then

    # Start mysql
    alias mysql_load="launchctl load -w /usr/local/Cellar/mysql/5.6.13/homebrew.mxcl.mysql.plist"
    alias mysql_unload="launchctl unload -w /usr/local/Cellar/mysql/5.6.13/homebrew.mxcl.mysql.plist"

  # Get the brew coreutils aliases (GNU stuff)
  GLSPATH=$(/usr/local/bin/brew --prefix coreutils)
  alias ls='$GLSPATH/bin/gls -lhGF --color=auto'

  # Quick way to rebuild the Launch Services database and get rid
  # of duplicates in the Open With submenu.
  alias fixopenwith='/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -kill -r -domain local -domain system -domain user'
fi

# Ruby and Gems
alias gs='gem search -r '
alias gi='gem install '

alias sz='source ~/.zshrc'
alias ez='vim ~/.zshrc'

alias b='bundle exec'

alias j=z

alias gl="git log --graph --abbrev-commit --decorate --date=relative --format=format:'%C(bold blue)%h%C(reset) - %C(bold green)(%ar)%C(reset)%C(white)%s%C(reset) %C(dim white)- %an%C(reset)%C(bold yellow)%d%C(reset)' --all"
#-----------------------------------------------------------------------
# Directory Aliases
hash -d ng=~/tmp/magneto/test/modules/mps_nagios_poller/files/nagios/conf.d.puppet
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

# LDAP -----------------------------------------------------------------
alias lsc='ldapsearch -LLL -h drc-wch-001 -D cn=root -w secret'

# Bower -----------------------------------------------------------------
alias bower='noglob bower'
