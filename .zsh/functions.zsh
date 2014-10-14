# FUNCTIONS -----------------------------------------------------

# mkdir && cd
function mcd()
{
	test -z "$1" && echo mcd: no path given && return
	test -d "$1" && print "mcd: Directory $1 already exists"
	mkdir -p -- "$1"
	cd -- "$1"
}

# Simple calculator
calc () {
    test -z "$1" && echo "usage example: calc \"3*6\"" && return
    #awk "BEGIN{ print $* }"
    echo $(($*))
}

# print the nth argument
col() { 
  awk -- "{print \$$1}";  
}

# only slash should be considered as a word separator:
slash-backward-kill-word() {
    local WORDCHARS="${WORDCHARS:s@/@}"
    # zle backward-word
    zle backward-kill-word
}
zle -N slash-backward-kill-word

# Kill everything in a word up to its last "/"
bindkey '^xv' slash-backward-kill-word

# press esc-m for inserting last typed word again (thanks to caphuso!)
insert-last-typed-word() { zle insert-last-word -- 0 -1 };
zle -N insert-last-typed-word

# Insert last typed word
bindkey "^xm" insert-last-typed-word

# run command line as user root via sudo:
sudo-command-line() {
    [[ -z $BUFFER ]] && zle up-history
    [[ $BUFFER != sudo\ * ]] && BUFFER="sudo $BUFFER"
    zle end-of-line
}
zle -N sudo-command-line

#k# Put the current command line into a \kbd{sudo} call
bindkey "^x^x" sudo-command-line

# Wrap the command line in a for statement
make-for-loop() {
    [[ -z $BUFFER ]] && zle up-history
    [[ $BUFFER != for\ * ]] && BUFFER="for x in ; do $BUFFER; done"
}
zle -N make-for-loop

bindkey "^xf" make-for-loop

### jump behind the first word on the cmdline.
### useful to add options.
function jump_after_first_word() {
    local words
    words=(${(z)BUFFER})

    if (( ${#words} <= 1 )) ; then
        CURSOR=${#BUFFER}
    else
        CURSOR=${#${words[1]}}
    fi
}
zle -N jump_after_first_word

bindkey '^x1' jump_after_first_word
