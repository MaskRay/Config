" -*- vim: set sts=2 sw=2 et fdm=marker: -------------  vim modeline -*-

" Basic Settings -------------------------------------- {{{1
syntax on
set nocompatible
filetype plugin indent on
let g:mapleader = " "

set hidden
set hlsearch
set incsearch
set ruler
set showcmd
set isfname-==
set shortmess+=s
set title
set wildcharm=<tab>
set wildmenu
set wildmode=list:longest,list:full
set wildignore=*.o,*.bak,*~,*.sw?,*.aux,*.toc,*.hg,*.git,*.svn,*.hi,*.so,*.a,*.pyc,*.aux,*.toc,*.exe
"set autochdir
set winaltkeys=no
set scrolloff=3 scrolljump=5
set sidescroll=3 sidescrolloff=3
"set ignorecase smartcase
set ttimeoutlen=100
set matchpairs=(:),[:],{:},<:>,':',":"
"au FileType c,cpp,java set mps+==:;

set backup
set backupdir=~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.tmp,~/tmp,/var/tmp,/tmp

set backspace=indent,eol,start
set history=200

set fileencodings=ucs-bom,utf8,cp936,gbk,big5,euc-jp,euc-kr,gb18130,latin1

set formatprg="par-format rTbgqR B=.,?_A_a Q=_s>|"
set guiheadroom=20
set grepprg=internal
"set grepprg=ack\ -a

" tabs and eols
set listchars+=tab:▸\ ,eol:¬
" spaces
set listchars+=trail:⋅,nbsp:⋅

if has('mouse')
  set mouse=a
endif

if has("gui_running")
"  set cursorcolumn
"  set cursorline
  set nowrap
  "set elativenumber
  " set spell

  " Ctrl-F12 Toggle Menubar and Toolbar
  nnoremap <silent> <C-F12> :
    \ if &guioptions =~# 'T' <Bar>
      \ set guioptions-=T <Bar>
      \ set guioptions-=m <Bar>
    \ else <Bar>
      \ set guioptions+=T <Bar>
      \ set guioptions+=m <Bar>
    \ endif<CR>

  set guioptions-=T
  set guioptions-=m
  " no scroll bars
  set guioptions-=r
  set guioptions-=L
endif

" Fonts ----------------------------------------------- {{{1
if has("gui_running")
  " Envy Code R
  " http://damieng.com/blog/2008/05/26/envy-code-r-preview-7-coding-font-released
  " set guifont=Monaco\ 15
  set guifont=Inconsolata\ 15
  set guifont=Monofur\ 16
  set guifontwide=WenQuanYi\ Micro\ Hei\ 13
endif

" Colorschemes ---------------------------------------- {{{1
" get them from: http://www.vim.org/
" also: http://code.google.com/p/vimcolorschemetest/ (recommanded)
if has("gui_running")
  " Darker Themes ------------------------------------- {{{2
  " ir_black
  " http://blog.infinitered.com/entries/show/8
  " http://blog.infinitered.com/entry_files/8/ir_black.vim
  "colorscheme ir_black
  "colorscheme fruity
  "colorscheme murphy
  "colorscheme peaksea
  " Lighter Themes ------------------------------------ {{{2
  "colorscheme sf
  "colorscheme settlemyer
  "colorscheme sea
  "colorscheme pyte
  "colorscheme pleasant
  "colorscheme navajo-night
  "colorscheme cloudy
  "colorscheme clarity
  "colorscheme mint
  "colorscheme kib_plastic
  "colorscheme jhlight
  " }}}2
else
  colorscheme bocau
endif

" Misc --------------------- {{{1
nnoremap zz zz:nohls<CR>
nnoremap <Leader>a :Ag
nnoremap <Leader>p "+p<CR>
nnoremap <Leader>P "+P<CR>
nnoremap <CR> i<CR><ESC>
noremap gz :bdelete<cr>

" Omni completion
inoremap <C-]> <C-x><C-]>
inoremap <C-F> <C-x><C-F>

" Edit
nnoremap <leader>ew :e <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>es :sp <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>ev :vsp <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>et :tabe <C-R>=expand("%:p:h") . "/" <CR>

" move in insert mode
inoremap <m-h> <left>
inoremap <m-l> <Right>
inoremap <m-j> <C-o>gj
inoremap <m-k> <C-o>gk

" Error navigation
nnoremap <m-down> :lnext<cr>zvzz
nnoremap <m-up> :lprevious<cr>zvzz
nnoremap <m-j> :cnext<cr>zvzz
nnoremap <m-k> :cprevious<cr>zvzz

" search for visual-mode selected text
vmap / y/<C-R>"<CR>

" Tabs
nnoremap tk :tabprevious<cr>
nnoremap tj :tabnext<cr>
nnoremap to :tabnew<cr>
nnoremap tq :tabclose<cr>

" vim hacks #159
nmap <leader>sj <SID>(split-to-j)
nmap <leader>sk <SID>(split-to-k)
nmap <leader>sh <SID>(split-to-h)
nmap <leader>sl <SID>(split-to-l)

nnoremap <SID>(split-to-j) :<C-u>execute 'belowright' (v:count == 0 ? '' : v:count) 'split'<CR>
nnoremap <SID>(split-to-k) :<C-u>execute 'aboveleft'  (v:count == 0 ? '' : v:count) 'split'<CR>
nnoremap <SID>(split-to-h) :<C-u>execute 'topleft'    (v:count == 0 ? '' : v:count) 'vsplit'<CR>
nnoremap <SID>(split-to-l) :<C-u>execute 'botright'   (v:count == 0 ? '' : v:count) 'vsplit'<CR>

nnoremap s <Nop>
nnoremap sh <C-w>h
nnoremap sj <C-w>j
nnoremap sk <C-w>k
noremap sl <C-w>l
nnoremap sH <C-w>H
nnoremap sJ <C-w>J
nnoremap sK <C-w>K
nnoremap sL <C-w>L

" vim hacks #181
" Open junk file."{{{
command! -nargs=0 JunkFile call s:open_junk_file()
function! s:open_junk_file()
  let l:junk_dir = '/tmp/.junk'. strftime('/%Y/%m')
  if !isdirectory(l:junk_dir)
    call mkdir(l:junk_dir, 'p')
  endif

  let l:filename = input('Junk Code: ', l:junk_dir.strftime('/%Y-%m-%d-%H%M%S.'))
  if l:filename != ''
    execute 'edit ' . l:filename
  endif
endfunction "}}}
nnoremap <leader>jf :JunkFile<cr>

" Beginning & End
noremap H ^
noremap L g_
inoremap <C-a> <esc>I
inoremap <C-e> <esc>A


" Open a Quickfix window for the last search.
nnoremap <silent> <leader>/ :execute 'vimgrep /'.@/.'/g %'<CR>:botright copen<CR>

" Save & Make
nnoremap <F5> :w<CR>:make!<CR><CR>:cc<CR>

" Paste toggle
set pastetoggle=<F7>

" visual shifting (does not exit Visual mode)
vnoremap < <gv
vnoremap > >gv

" Buffer
nnoremap <C-Tab> :bn<cr>
nnoremap <C-S-Tab> :bp<cr>

" Edit
cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>ew :e %%
map <leader>es :sp %%
map <leader>ev :vsp %%
map <leader>et :tabe %%

" Scrolling
map zl zL
map zh zH

" ; :
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

nmap <Leader>fw [I:let nr = input("Which one: ")<Bar>exe "normal " . nr ."[\t"<CR>

" Command-line editing
cnoremap <C-R><C-L> <C-R>=getline('.')<CR>

cmap w!! w !sudo tee % >/dev/null

let g:haddock_browser = "firefox"
autocmd BufRead *.hs setlocal equalprg=~/bin/pp-haskell.hs
nnoremap <F10> :set wrap!<CR>

let g:Tex_Flavor='latex'
let g:Tex_CompileRule_pdf = 'xelatex -interaction=nonstopmode $*'

func! C_init()
  set tags+=~/.vim/static/cpp                        " core in cpp
  abbr #i #include
  set syntax=cpp11.doxygen
  let &makeprg="clang++ % -g -Wall -Wextra -O0 -std=c++11 -o %<"
  syn keyword cppType real_t Vec Vec2D Vector Matrix Plane Sphere Geometry Ray Color Img imgptr
  syn keyword cppSTL priority_queue hypot isnormal isfinite isnan shared_ptr make_shared numeric_limits move
  syn keyword cppSTLType T
endfunc

func! JS_init()
  nnoremap <buffer> [D :TernDef<CR>
  nnoremap <buffer> [I :TernRefs<CR>
endfunc

func! Ruby_init()
  let &makeprg="ruby -c %"
  imap <C-CR> <CR><CR>end<Esc>-cc
endfunc


func! Make()						" silent make with quickfix window popup
	if &ft == 'cpp'
		if filereadable(getcwd() . "/Makefile")
			let &makeprg="make"
		elseif  filereadable(getcwd() . "/../Makefile")
			let &makeprg="make -C .."
		endif
	endif
	make
	" silent make ?
	redraw!
	for i in getqflist()
		if i['valid']
			cwin | winc p | return
		endif
	endfor
endfunc

nnoremap <Leader>mk :call Make()<CR>

nmap <Leader>nw :set wrap!<CR>

" Plugins --------------------------------------------- {{{1
" Vundle ---------------------------------------------- {{{2
if has("gui_running")
  filetype off
  set rtp+=~/.vim/bundle/vundle/
  call vundle#rc()

  Bundle 'gmarik/vundle'
  Bundle 'Tabular'
  Bundle 'EasyMotion'
  Bundle 'syntastic'
  "Bundle 'UltiSnips'
  Bundle 'vim-PinyinSearch'
  Bundle 'YankRing'
  Bundle 'dispatch'
  "Bundle 'vimproc'
  "Bundle 'vimshell'
  Bundle 'unite.vim'
  Bundle 'unite-outline'
  Bundle 'SingleCompile'
  Bundle 'vim-unimpaired'
  Bundle 'gundo'
  Bundle 'ag'
  Bundle 'LaTeX-Box'
  Bundle 'VisIncr'
  Bundle 'colorv.vim'
  Bundle 'startify'
  Bundle 'splitjoin'

  Bundle 'vimside'

  "Bundle 'R-plugin'

  Bundle 'YouCompleteMe'
  let g:ycm_global_ycm_extra_conf = $HOME . "/.vim/static/ycm_extra_conf.py"
  let g:ycm_key_detailed_diagnostics = "<Leader>yd"
  let g:ycm_key_invoke_completion = "<F5>"
  let g:ycm_complete_in_comments = 1
  let g:ycm_collect_identifiers_from_tags_files = 1
  let g:ycm_seed_identifiers_with_syntax = 1
  let g:ycm_autoclose_preview_window_after_completion = 1
  let g:ycm_autoclose_preview_window_after_insertion = 1
  let g:ycm_key_list_select_completion = ['<TAB>', '<Down>', '<Enter>']
  let g:ycm_confirm_extra_conf = 0
  let g:ycm_cache_omnifunc = 0
  let g:ycm_filetype_blacklist = {'markdown' : 1,  'txt' : 1, 'help' : 1}

  Bundle 'css3-syntax'
  Bundle 'matchit'
  Bundle 'stylus'
  Bundle 'jade'
  Bundle 'coffee-script'
  Bundle 'jsbeautify'
  Bundle 'tern_for_vim'

  Bundle 'evanmiller/nginx-vim-syntax'
  Bundle 'markdown'

  Bundle 'vim-ruby'
  Bundle 'slim'

  "Bundle 'python-mode'
  Bundle 'jedi'
  Bundle 'pytest.vim'

  filetype plugin indent on    " required!
endif
