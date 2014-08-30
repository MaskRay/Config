" -*- vim: set sts=2 sw=2 et fdm=marker: -------------  vim modeline -*-

" Basic Settings -------------------------------------- {{{1
syntax on
set nocompatible
filetype plugin indent on
let g:mapleader = " "

set sw=2 sts=2 et
set display=lastline
set hidden
set hlsearch
set incsearch
set nrformats=hex
set ruler
set showcmd
set isfname-==
set shortmess+=s
set title
set whichwrap=b,s,[,]
set wildcharm=<tab>
set wildmenu
set wildmode=list:longest,list:full
set wildignore=*.o,*.bak,*~,*.sw?,*.aux,*.toc,*.hg,*.git,*.svn,*.hi,*.so,*.a,*.pyc,*.aux,*.toc,*.exe
"set autochdir
set winaltkeys=no
set scrolloff=3 scrolljump=5
set sidescroll=10 sidescrolloff=20
set switchbuf=useopen
"set ignorecase smartcase
set timeoutlen=300
set ttimeoutlen=0
set matchpairs=(:),[:],{:},<:>,':',":"

"set backup
"set backupdir=~/tmp,/var/tmp,/tmp
"set directory=~/tmp,/var/tmp,/tmp
set nobackup noswapfile
if has('persistent_undo')
  set undofile
  set undolevels=200
  set undodir=~/.vimtmp/undo
end
set viminfo+=n~/.vimtmp/viminfo

" set backspace=indent,eol,start
set history=200

set fileencodings=ucs-bom,utf8,cp936,gbk,big5,euc-jp,euc-kr,gb18130,latin1

set formatprg="par-format rTbgqR B=.,?_A_a Q=_s>|"
set formatoptions+=n " support formatting of numbered lists
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
" nnoremap zz zz:nohls<CR>
nnoremap <silent> <C-l> :nohls<cr><C-l>
nnoremap <Leader>a :Ag<space>
nnoremap <Leader>p "+p<CR>
nnoremap <Leader>P "+P<CR>
nnoremap <CR> i<CR><ESC>
noremap gz :bdelete<cr>

ino <C-j> <C-r>=TriggerSnippet()<cr>

" Omni completion
" inoremap <C-]> <C-x><C-]>
" inoremap <C-F> <C-x><C-F>

" Edit
cnoremap <expr> %% getcmdtype() == ':' ? expand('%:h').'/' : '%%'
nnoremap <leader>ee :e <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>es :sp <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>ev :vsp <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>et :tabe <C-R>=expand("%:p:h") . "/" <CR>
nnoremap <leader>w :set wrap!<CR>

" Diff
nnoremap <leader>dt :diffthis<CR>
nnoremap <leader>do :bufdo diffoff<CR>

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
vmap X y/<C-R>"<CR>

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

" Open a Quickfix window for the last search.
nnoremap <silent> <leader>/ :execute 'vimgrep /'.@/.'/g %'<CR>:botright copen<CR>

" Paste toggle
set pastetoggle=<F7>

" visual shifting (does not exit Visual mode)
vnoremap < <gv
vnoremap > >gv

" Buffer
nnoremap <C-Tab> :bn<cr>
nnoremap <C-S-Tab> :bp<cr>

xnoremap <C-c> "+y
"inoremap <C-v> <esc>:se paste<cr>"+p:se nopaste<cr>i

" insert word of the line above
inoremap <C-Y> <C-C>:let @z = @"<CR>mz
           \:exec 'normal!' (col('.')==1 && col('$')==1 ? 'k' : 'kl')<CR>
           \:exec (col('.')==col('$') - 1 ? 'let @" = @_' : 'normal! yw')<CR>
           \`zp:let @" = @z<CR>a

au BufReadPost *
      \ if line("'\"") > 0 && line("'\"") <= line("$") |
      \   exe "normal g`\"" |
      \ endif

nnoremap <Leader>cp :!xsel -ib < %<cr><cr>
nnoremap <Leader>bk :!cp % ~/tmp/%.bak --backup=numbered<cr>
nnoremap <leader>ig :IndentLinesToggle<cr>:se list!<cr>

" ; :
nnoremap ; :
nnoremap : ;
vnoremap ; :
vnoremap : ;

" surf
nnoremap <leader>sf :Surf<cr>

" SplitJoin
nnoremap <leader>sj :SplitJoinJoin<cr>
nnoremap <leader>sJ :SplitJoinSplit<cr>

nmap <Leader>fw [I:let nr = input("Which one: ")<Bar>exe "normal " . nr ."[\t"<CR>
nnoremap <S-F12> :!gtags && ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>

" Command-line editing
cnoremap <C-R><C-L> <C-R>=getline('.')<CR>

cmap w!! w !sudo tee % >/dev/null

let g:haddock_browser = "firefox"
autocmd BufRead *.hs setlocal equalprg=~/bin/pp-haskell.hs

au BufEnter *.cpp let b:fswitchdst = 'hh,h' | let b:fswitchlocs = './,../include'
au BufEnter *.cc let b:fswitchdst = 'hh,h' | let b:fswitchlocs = '.,../include'
au BufEnter *.hh let b:fswitchdst = 'cpp,cc' | let b:fswitchlocs = '.'
au BufEnter *.h let b:fswitchdst = 'cpp,cc' | let b:fswitchlocs = '.'
command! A FSHere
command! AV FSSplitRight

let g:Tex_Flavor='latex'
let g:Tex_CompileRule_pdf = 'xelatex -interaction=nonstopmode $*'

func! C_init()
  setl tags+=~/.vim/static/cpp                        " core in cpp
  setl dictionary=~/.vim/dict/cpp
  abbr #i #include
  set syntax=cpp11.doxygen
  let &makeprg="clang++ % -g -Wall -Wextra -O0 -std=c++11 -o %<"
  syn keyword cppType u real_t Vec Vec2D Vector Matrix Plane Sphere Geometry Ray Color Img imgptr
  syn keyword cppSTL priority_queue hypot isnormal isfinite isnan shared_ptr make_shared numeric_limits move
  syn keyword cppSTLType T
endfunc

func! CSS_init()
  setl dictionary=~/.vim/dict/css
endfunc

func! JS_init()
  nnoremap <buffer> [D :TernDef<CR>
  nnoremap <buffer> [I :TernRefs<CR>
endfunc

func! Ruby_init()
  let &makeprg="ruby -c %"
  imap <C-CR> <CR><CR>end<Esc>-cc
  nnoremap <silent><buffer> ]] :RubyJumpForward<cr>
  nnoremap <silent><buffer> [[ :RubyJumpBackward<cr>
  nnoremap <silent><buffer> ]<space> :RubyJump<cr>
endfunc

func! Fortran_init()
  let g:fortran_free_source = #1
endfunc

func! Tex_init()
	" pdf auto refresh preview
	au BufWritePost *.tex call system("zsh -c 'pgrep -a xelatex || make; killall -SIGHUP mupdf > /dev/null 2 >&1' &")

    setl nocursorline                                " for performance
    hi clear Conceal
    setl sw=2 sts=2 expandtab
    setl textwidth=150
    setl errorformat=aaaaaaa                        " disable quickfix
    setl fo-=q
    setl cole=0

    inoremap <buffer> $$ $<Space>$<Left>
    inoremap <buffer> " ``''<Left><Left>
    nmap <buffer> <Leader>" xi``<Esc>,f"axi''<Esc>
    inoremap <buffer> ... \cdots<Space>

    inoremap <buffer> \[ \[<Space>\]<Left><Left>
    inoremap <buffer> \{ \{<Space>\}<Left><Left>
    inoremap <buffer> \langle \langle<Space><Space>\rangle<Esc>7hi
    inoremap <buffer> \verb \verb<Bar><Bar><Left>
    inoremap <buffer> \beg \begin{}<Left>
    inoremap <buffer> \bb <Esc>:call Tex_Block("")<Left><Left>
    inoremap <buffer> \bbt <Esc>:call Tex_Block("t")<CR><Up><End>[H]<Down>\centering<CR>\caption{\label{tab:}}<Esc>k:call Tex_Block("tabular")<CR>
    inoremap <buffer> \bbf <Esc>:call Tex_Block("f")<CR><Up><End>[H]<Down>\centering<CR>\includegraphics[width=\textwidth]{res/}<CR>\caption{\label{fig:}}<Esc>
    inoremap <buffer> \bbm <Esc>:call Tex_Block("mp")<CR><Up><End>[b]{0.46\linewidth}<Down>\centering<CR>\includegraphics[width=\textwidth]{res/}<CR>\caption{\label{fig:}}<Esc>
    inoremap <buffer> \bf \textbf{}<Left>
    xmap <buffer> \bbe di\bbe<CR><Tab><Esc>pj
    xmap <buffer> \bbd di\bbd<CR><Tab><Esc>pj
    xmap <buffer> \bf s}i\textbf<Esc>
    xmap <buffer> \em s}i\emph<Esc>
    xmap <buffer> <Leader>tab :s/\s\+/ \&/g<CR>gv:s/$/\\\\/g<CR>gv<Space>tt

    " Plugin: LaTeX-Box
    let g:LatexBox_no_mappings = 1
    inoremap <buffer> [[ \begin{}<Left>
    imap <buffer> ]] <Plug>LatexCloseCurEnv
    inoremap <buffer> <C-n> <Esc><Right>:call LatexBox_JumpToNextBraces(0)<CR>i
    nmap <buffer> P l:call LatexBox_JumpToNextBraces(0)<CR>
    nmap <buffer> Q :call LatexBox_JumpToNextBraces(1)<CR>

    xmap <buffer> ie <Plug>LatexBox_SelectCurrentEnvInner
    xmap <buffer> ae <Plug>LatexBox_SelectCurrentEnvOuter
    omap <buffer> ie :normal vie<CR>
    omap <buffer> ae :normal vae<CR>
    xmap <buffer> im <Plug>LatexBox_SelectInlineMathInner
    xmap <buffer> am <Plug>LatexBox_SelectInlineMathOuter
    omap <buffer> im :normal vim<CR>
    omap <buffer> am :normal vam<CR>

    nmap <buffer> <Leader>ce <Plug>LatexChangeEnv
    xmap <buffer> <Leader>tc <Plug>LatexWrapSelection
    xmap <buffer> <Leader>te <Plug>LatexEnvWrapSelection
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

au BufNewFile,BufRead *.txt,*.doc,*.pdf setl ft=txt
au BufReadPre *.doc,*.class,*.pdf setl ro
au BufReadPost *.doc silent %!antiword "%"
au BufRead *.class exe 'silent %!javap -c "%"' | setl ft=java
au BufReadPost *.pdf silent %!pdftotext -nopgbrk "%" -

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
  Bundle 'UltiSnips'
  Bundle 'vim-PinyinSearch'
  "Bundle 'YankRing'
  Bundle 'dispatch'
  "Bundle 'vimproc'
  "Bundle 'vimshell'
  Bundle 'unite.vim'
  Bundle 'unite-outline'
  Bundle 'vim-unimpaired'
  Bundle 'gundo'
  Bundle 'ag'
  Bundle 'LaTeX-Box'
  Bundle 'VisIncr'
  Bundle 'colorv.vim'
  Bundle 'startify'
  Bundle 'splitjoin'
  Bundle 'commentary'
  Bundle 'eunuch'
  Bundle 'ZoomWin'
  Bundle 'fswitch'
  Bundle 'polyglot'
  Bundle 'exchange'
  Bundle 'nerdcommenter'

  "Bundle 'vimside'

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
  let g:ycm_auto_trigger = 0

  Bundle 'css3-syntax'
  Bundle 'matchit'
  Bundle 'stylus'
  Bundle 'jade'
  Bundle 'coffee-script'
  Bundle 'tern_for_vim'
  Bundle 'coloresque'

  Bundle 'evanmiller/nginx-vim-syntax'
  Bundle 'markdown'

  Bundle 'vim-ruby'
  Bundle 'rubyjump'
  Bundle 'vim-rails'
  Bundle 'slim'

  "Bundle 'python-mode'
  Bundle 'jedi'
  Bundle 'pytest.vim'

  "Bundle 'emmet'
  Bundle 'surfer'

  Bundle 'vim-airline'

  filetype plugin indent on    " required!
endif

let g:fortran_free_source = 1
