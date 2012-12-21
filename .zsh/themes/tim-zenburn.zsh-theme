# tim prompt theme

PROMPT='%{$fg[blue]%}%n%{$fg[bright_white]%}@%{$fg[green]%}%m %{$fg[red]%}[%~] %{$reset_color%}
%{$fg[bright_white]%}> %{$reset_color%}'

# ZSH_THEME_GIT_PROMPT_PREFIX="git:(%{$fg[yellow]%}"
# ZSH_THEME_GIT_PROMPT_SUFFIX="%{$reset_color%}"
# ZSH_THEME_GIT_PROMPT_DIRTY="%{$fg[blue]%}) %{$fg[yellow]%}✗%{$reset_color%}"
# ZSH_THEME_GIT_PROMPT_CLEAN="%{$fg[blue]%})"

#prompt_tim_setup () {
#  prompt_tim_blue='blue'
#  prompt_tim_grey='grey'
#  prompt_tim_green='green'
#  prompt_tim_red='red'
#
#  base_prompt="%F{$prompt_tim_blue}%n%F{$prompt_tim_grey}@%F{$prompt_tim_green}%m "
#  post_prompt="%f"
#  path_prompt="%F{$prompt_tim_red}[%~]
#%F{white}"
#
#  PS1="$base_prompt$path_prompt%# $post_prompt"
#  PS2="%F{$prompt_tim_red}> $post_prompt"
#  PS3="$base_prompt$path_prompt?# $post_prompt"
#}
#
#prompt_tim_setup "$@"

LS_COLORS='di=38;5;108:fi=00:*svn-commit.tmp=31:ln=38;5;116:ex=38;5;186'
export LS_COLORS

