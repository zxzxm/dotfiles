" This expects a "~/.vim" directory where all plugins and so forth are
" installed.  In my case Janus (https://github.com/carlhuda/janus.git)

" Before loading Janus, set the leader
let mapleader = ","

noremap <silent> <CR> :nohlsearch<CR>

" Load janus
if filereadable(expand("~/.vim/vimrc"))
	source ~/.vim/vimrc
endif
