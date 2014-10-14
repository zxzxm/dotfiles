#-----------------------------------------------------------------------
# OPTIONS
#

# Don't push the same dir twice.
setopt pushd_ignore_dups

# Don't match dotfiles. ever.
setopt noglobdots

# Use zsh style word splitting
setopt noshwordsplit

# If command cannot be executed and name is a directory, cd to it
setopt auto_cd

# Use #, ~, and ^ for filename generation
setopt extended_glob

# Disable CTRL-S
setopt no_flow_control

# beeps are annoying
setopt no_beep

# Keep echo "file" > file from clobbering file
#setopt NO_CLOBBER

# Case insensitive globbing
setopt no_case_glob

# Be Reasonable!
setopt numeric_glob_sort

# I don't know why I never set this before.
setopt extended_glob

# hows about arrays be awesome?  (that is, frew${cool}frew has frew surrounding all the variables, not just first and last
setopt rc_expand_param

# Use Emacs keys
bindkey -e

# No username completion
setopt no_cdable_vars
