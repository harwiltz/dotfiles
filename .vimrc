" SETTINGS
" =================================================================================================
set background=dark
set t_Co=256
set list listchars=tab:\‣\ ,eol:¬,trail:•
set path+=** "Recursive :find
set wildmenu
set wildignore+=*.o,*.obj,.git,venv/**/*,*.pyc,__pycache__,node_modules,build/**/*,dist/**/*
set omnifunc=syntaxcomplete#Complete
set laststatus=2
set incsearch
set showmode
set nohlsearch
set fdm=manual
set termguicolors
set statusline=
set statusline+=%#statusNormalFile#
set statusline+=%F\ |
set statusline+=%#statusNormal#
set nocompatible
set clipboard=unnamedplus
set number
set relativenumber
set autoindent
set smartindent
set ruler " Line number and cursor pos
set cindent
set cursorline
set list
set expandtab
set tabstop=2
set shiftwidth=2

syntax on
filetype off
filetype plugin indent on
colorscheme solarized8
" -------------------------------------------------------------------------------------------------

" VARIABLES
" =================================================================================================
let g:tex_flavor='tex'
let g:clang_cpp_options = '-std=c++11 -stdlib=libc++ -stdlib=sdl2'
let g:mode = 0
let g:BASH_Ctrl_j = 'off'
" This is only necessary if you use "set termguicolors".
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
" -------------------------------------------------------------------------------------------------

" AUTOCMD
" =================================================================================================
au BufWritePost *.tex call CompileTex()

au InsertEnter * call ToInsert()
au InsertLeave * call FromInsert()

autocmd BufNewFile,BufRead *.hs set expandtab
autocmd BufNewFile,BufRead *.hs set shiftwidth=4
autocmd BufNewFile,BufRead *.hs set tabstop=4
autocmd BufNewFile,BufRead **/*.elm :set filetype=haskell

autocmd BufNewFile,BufRead *.tex Wrap()
autocmd BufNewFile,BufRead *.tex set tw=100
autocmd BufNewFile,BufRead *.md Wrap()
autocmd BufNewFile,BufRead *.markdown set tw=100
autocmd BufNewFile,BufRead *.markdown Wrap()
autocmd BufNewFile,BufRead *.md set tw=100

autocmd BufWinLeave *.* mkview
autocmd BufWinEnter *.* silent loadview
" -------------------------------------------------------------------------------------------------

" MAPS
" =================================================================================================
nmap <leader>f :e **/*
nmap <leader>l /\s\+$\\| \+\ze\t<CR>
nnoremap <leader>c :execute "set cc=" . (&cc == "100" ? "" : "100")<CR>
"Move line up/down
noremap <Down> ddp
nnoremap <C-S-K> ddkkp
nnoremap <Up> ddkkp
vnoremap <C-S-J> :m '>+1<CR>gv=gv
vnoremap <Down> :m '>+1<CR>gv=gv
vnoremap <C-S-K> :m '<-2<CR>gv=gv
vnoremap <Up> :m '<-2<CR>gv=gv
"Selecting a block
nmap <C-S-B> j]}v[{k
map <C-U> :call Comment()<CR>j
imap <C-U> <ESC>:call Comment()<CR>i
" Copy/paste
vnoremap <C-C> "+y
nnoremap <C-P> "+p
inoremap <C-V> <ESC>"+pi
nmap <C-S-P> :call <SID>SynStack()<CR>

"SNIPPETS
nnoremap \eq :read /home/harwiltz/.vim/snippets/equation.tex<CR>jo
" -------------------------------------------------------------------------------------------------

function! DrawStatus(mode)
	if &readonly
		set statusline+=%#statusReadOnly#
		set statusline+=Caution:\ READONLY
	endif
	set statusline+=\ %m
	set statusline+=\ [%p%%]
	set statusline+=%=
	set statusline+=%#statusLocation#
	set statusline+=[%3c:%4l/%-4L]
endfunction
call DrawStatus(0)

function! ToInsert()
	let g:mode = 1
	set statusline=
	set statusline+=%#statusInsertFile#
	set statusline+=%F\ | 
	set statusline+=%#statusInsert#
	call DrawStatus(1)
endfunction

function! FromInsert()
	let g:mode = 0
	set statusline=
	set statusline+=%#statusNormalFile#
	set statusline+=%F\ |
	set statusline+=%#statusNormal#
	call DrawStatus(0)
endfunction



command! -nargs=* Stab call Stab()
function! Stab()
	let l:tabstop = 1 * input('set tab size: ')
	if l:tabstop > 0
		let &l:sts = l:tabstop
		let &l:sw = l:tabstop
		let &l:ts = l:tabstop
	else
		echon 'ERROR - Tab size must be > 0'
	endif
endfunc

command! -nargs=* Wrap set wrap linebreak
command! -nargs=* Unwrap call Unwrap()
function! Unwrap()
	set wrap
	set linebreak!
	set nolist!
endfunc


function! <SID>SynStack()
	if !exists("*synstack")
		return
	endif
	echo map(synstack(line('.'),col('.')), 'synIDattr(v:val, "name")')
endfunc

function! Comment()
	let line = getline('.')
	let linenumber = line('.')
	let type = &ft
	let comment = "//"
	let len = 2
	if type == "vim"
		let comment = "\""
		let len = 1
	elseif type == "haskell"
		let comment = "--"
		let len = 2
	elseif type == "xdefaults"
		let comment = "!"
		let len = 1
	elseif type == "zsh"
		let comment = "#"
		let len = 1
	elseif type == "sh"
		let comment = "#"
		let len = 1
	elseif type == "conf"
		let comment = "#"
		let len = 1
	elseif type == "python"
		let comment = "#"
		let len = 1
	elseif type == "tex"
		let comment = "%"
		let len = 1
	elseif type == "vhdl"
		let comment = "--"
		let len = 2
	elseif type == "matlab"
		let comment = "%"
		let len = 1
	endif

	let pos = match(line,comment)
	let n = 0

	while n < 100
		if line[n] != ' ' && line[n] != '\t'
			break
		endif

		let n = n + 1
	endwhile

	if n == pos && pos != -1
		let line = strpart(line,0,pos).strpart(line,pos+len)
	else
		let line = comment.line
"		let line = strpart(line,0,n).(comment.strpart(line,n))
	endif

	let err = setline(linenumber,line)
endfunction
" This doesn't work.
function! Init()
	let num = line('.')
	let line = getline('.')
"	let err = setline(num,line)
	let type = &ft
	let line = line.type
	if type == "haskell"
"		let line = getline('.')
"		let line = line."THIS IS HASKELL"
		set expandtab
	endif
endfunction

function! CompileTex()
	let firstLine = getline(1)
	let cur_line = getpos('.')
	if search("%notfulltexdoc") == 0
		if firstLine == "%xetex"
			!xelatex %
		else
			!pdflatex %
		endif
	else
		call setpos('.', cur_line)
	endif
endfunction
