" -*- vim: set sts=2 sw=2 et fdm=marker: -------------  vim modeline -*-

" Basic Settings -------------------------------------- {{{1
syntax on
set nocompatible
filetype plugin indent on

set hlsearch
set incsearch
set ruler
set showcmd
set number
set shiftround
set title
set wildmenu
set wildmode=list,longest
set wildignore=*.o,*.bak,*~,*.sw?,*.aux,*.toc,*.hg,*.git,*.svn,*.hi,*.so,*.a
set autochdir
set winaltkeys=no
set listchars=nbsp:¬,eol:¶,tab:>-,extends:»,precedes:«,trail:•
set scrolloff=3

set backup
set backupdir=~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.tmp,~/tmp,/var/tmp,/tmp

set backspace=indent,eol,start
set history=200

set laststatus=2
set statusline=%<%#ColorColumn#%2n%*»%#DiffChange#%{getcwd()}/%*%#DiffAdd#%f%*%#DiffText#%m%r%*»%#Title#%y%*»%#CursorLine#(%l/%L,%c)%*»%=«%#Cursor#%02B%*«%#ErrorMsg#%o%*«%#ModeMsg#%3p%%%*
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

set fileencodings=ucs-bom,utf8,cp936,gbk,big5,euc-jp,euc-kr,gb18130,latin1
set dictionary+=/usr/share/dict/words
set grepprg=ack\ -a
set whichwrap=b,s,<,>,[,],h,l
set showbreak=↪
set spellsuggest=10
let mapleader = ","

if version >= 703
    set undodir=~/.vim/undo
    set undofile
    set undolevels=100
    set undoreload=1000
endif

if has('mouse')
  set mouse=a
endif

if has("gui_running")
  set nowrap
  set spell

  " Ctrl-F12 Toggle Menubar and Toolbar
  nmap <silent> <C-F12> :
    \ if &guioptions =~# 'T' <Bar>
      \ set guioptions-=m <Bar>
    \ else <Bar>
      \ set guioptions+=m <Bar>
    \ endif<CR>

  set guioptions-=T
  set guioptions-=m
  set guioptions-=r
endif

" Mark extra whitespaces in red
highlight ExtraWhitespace ctermbg=red guibg=red
match ExtraWhitespace /\s\+$/

" Fonts ----------------------------------------------- {{{1
if has("gui_running")
  " Envy Code R
  " http://damieng.com/blog/2008/05/26/envy-code-r-preview-7-coding-font-released
  " set guifont=Monaco\ 15
  set guifont=Inconsolata\ 17
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
  colorscheme molokai
  hi Normal       guifg=White guibg=Black
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

" Autocommands ---------------------------------------- {{{1
if has("autocmd")
  augroup vimrcEx
  au!

  " C Support ----------------------------------------- {{{2
  augroup c_support
    set cinoptions+=:0,g0

  " Shell Support ------------------------------------- {{{2
  autocmd BufNewFile *.sh 0put=\"#!/bin/bash\<nl># vim:fdm=marker\<nl>\"
  au BufWritePost * if getline(1) =~ "^#!/bin/[a-z]*sh" | silent !chmod a+x <afile> | endif

  autocmd BufWritePost    * call Lilydjwg_chmodx()

  " Ruby Support -------------------------------------- {{{2
  augroup ruby_support
    au!
    autocmd FileType ruby inoreab <buffer> #! #!/usr/bin/env ruby
    autocmd FileType ruby inoreab <buffer> #e # coding: utf-8

  " Python Support ------------------------------------ {{{2
  augroup python_support
    au!
    autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
    autocmd FileType python inoreab <buffer> #! #!/usr/bin/env python
    autocmd FileType python inoreab <buffer> #e # -*- coding: utf-8 -*-

  " Java Support -------------------------------------- {{{2
  augroup java_suppoer
    au!
    autocmd FileType *.java setlocal omnifunc=javacomplete#Complete

  " Abbreviations for vim modeline -------------------- {{{2
  augroup vim_modeline
    au!
    autocmd FileType c,cpp,vala,genie inoreab <buffer> /*v /* -*- vim: set sts=4 sw=4 et fdm=marker tw=78 ----------- vim modeline -*- */
    autocmd FileType c,cpp,vala,genie inoreab <buffer> //v // -*- vim: set sts=4 sw=4 et fdm=marker tw=78 ----------- vim modeline -*-
    autocmd FileType python inoreab <buffer> #v # -*- vim: set sts=4 sw=4 et fdm=marker tw=72: -------- vim modeline -*-
    autocmd FileType rst inoreab <buffer> ..v .. -*- vim: set sts=2 sw=2 et fdm=marker: ---------------- vim modeline -*-
    autocmd FileType html,htmldjango inoreab <buffer> {#v {# -*- vim: set sts=2 sw=2 et fdm=marker ft=htmldjango: -- vim modeline -*- #}
    autocmd FileType lisp,scheme inoreab <buffer> ;;v ;; -*- vim: set fdm=marker: ------------------------------ vim modeline -*-

  " Default tab settings for different file types ----- {{{2
  augroup tab_settings
    au!
    autocmd FileType asciidoc setlocal sw=2 sts=2
    autocmd FileType c setlocal sw=4 sts=4 et
    autocmd FileType cpp setlocal sw=4 sts=4 et
    autocmd FileType css setlocal sw=4 sts=4 et
    autocmd FileType haskell setlocal sw=4 sts=4 et
    autocmd FileType html setlocal sw=2 sts=2 et
    autocmd FileType htmlcheetah setlocal sw=2 sts=2 et
    autocmd FileType htmldjango setlocal sw=2 sts=2 et
    autocmd FileType java setlocal sw=4 sts=4 et
    autocmd FileType javascript setlocal sw=2 sts=2 et
    autocmd FileType make set noet
    autocmd FileType mason setlocal sw=2 sts=2 et
    autocmd FileType ocaml setlocal sw=2 sts=2 et
    autocmd FileType perl setlocal sw=4 sts=4 et
    autocmd FileType php setlocal sw=4 sts=4 et
    autocmd FileType rst setlocal sw=2 sts=2 et
    autocmd FileType ruby setlocal sw=2 sts=2 et
    autocmd FileType python setlocal sw=4 sts=4 et tw=72
    autocmd FileType scheme setlocal sw=2 sts=2 et
    autocmd FileType sql setlocal et
    autocmd FileType vala setlocal sw=4 sts=4 et
    autocmd FileType xhtml setlocal sw=2 sts=2 et
    autocmd FileType xml setlocal sw=2 sts=2 et
    autocmd FileType text setlocal textwidth=78

  " Show trailing whitespaces when necessary ---------- {{{2
  " That is, most of the cases other than editing source code in Whitespace,
  " the programming language.
  augroup show_whitespaces
    au!
    " Make sure this will not be cleared by colorscheme
    autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
    autocmd BufWinEnter * match ExtraWhitespace /\s\+$/
    autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
    autocmd InsertLeave * match ExtraWhitespace /\s\+$/
    autocmd BufWinLeave * call clearmatches()

  " Misc ---------------------------------------------- {{{2
  augroup editing
    au!
    " Toggling between number and relativenumber when entering/leaving insert mode
    autocmd InsertEnter * set number
    autocmd InsertLeave * set relativenumber
    " remove trailing whitespaces
    autocmd BufWritePre * call StripTrailingWhitespace()

    " When editing a file, always jump to the last known cursor position.
    " Don't do it when the position is invalid or when inside an event handler
    " (happens when dropping a file on gvim).
    " Also don't do it when the mark is in the first line, that is the default
    " position when opening a file.
    autocmd BufReadPost *
      \ if line("'\"") > 1 && line("'\"") <= line("$") |
      \   exe "normal! g`\"" |
      \ endif

    " turn on spell checker for email
    autocmd FileType mail setlocal spell

  augroup END " --------------------------------------- }}}2
else
  set autoindent
endif " has("autocmd")

" Plugins --------------------------------------------- {{{1
" Vundle ---------------------------------------------- {{{2
filetype off
set rtp+=~/.vim/bundle/vundle/
call vundle#rc()
Bundle 'gmarik/vundle'
Bundle 'vim-powerline'
Bundle 'ctrlp'
Bundle 'tabular'
Bundle 'neco-ghc'
Bundle 'syntastic'
Bundle 'EasyMotion'
Bundle 'javacomplete'
Bundle 'vim-sparkup'
"Bundle 'rainbow_parentheses'
Bundle 'tasklist'
filetype plugin indent on    " required!
" EasyMotion ------------------------------------------ {{{2
let g:EasyMotion_do_mapping = 1
let g:EasyMotion_leader_key = g:mapleader

" FuzzyFinder ----------------------------------------- {{{2
" vim-l9 is the requirement of fuzzyfinder 4.*
" http://www.vim.org/scripts/script.php?script_id=3252
" http://bitbucket.org/ns9tks/vim-l9/
"set runtimepath+=~/projects/vim-l9/
" http://www.vim.org/scripts/script.php?script_id=1984
" http://bitbucket.org/ns9tks/vim-fuzzyfinder/
" mapping for FuzzyFinder
" use V 3.4
"set runtimepath+=~/projects/vim-fuzzyfinder/
"map <F3> :FufFile ~/projects/<CR>
" search from cwd
map <F3> :FufFile<CR>
map <F4> :FufBuffer<CR>

" NERDTree -------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=1658
" https://github.com/scrooloose/nerdtree
map <F2> :NERDTreeToggle<CR>

" SuperTab -------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=1643
" let SuperTabContextDefaultCompletionType = "context"

" dot.vim --------------------------------------------- {{{2

" Tagbar ---------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=3465
" http://github.com/majutsushi/tagbar
nnoremap <silent> <F8> :TagbarToggle<CR>

" Taglist --------------------------------------------- {{{2
" http://vim-taglist.sourceforge.net/
nnoremap <silent> <C-F8> :TlistToggle<CR>
map <F12> :!ctags -R --c++-kinds=+p --fields=+iaS --extra=+q .<CR>
" add python tags
" ctags file is generated like this:
" ctags -R -f ~/.vim/tags/python.ctags --c-kinds=+p --fields=+S /usr/lib/python2.6/
"set tags+=$HOME/.vim/tags/python.ctags
" for C/C++ with signature
" ctags -R -f ~/.vim/tags/c.ctags --c-kinds=+p --fields=+S /usr/include/
" ctags -R -f ~/.vim/tags/cpp.ctags --c++-kinds=+p --fields=+iaS --extra=+q /usr/include
"set tags+=$HOME/.vim/tags/cpp.ctags

" TaskList -------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=2607
map <F9> <Plug>TaskList

" VimIm ----------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=2506
" vimim settings, show menu background color
"let g:vimim_wildcard_search=1
"let g:vimim_sexy_onekey=1
"let g:vimim_chinese_frequency=10
"let g:vimim_custom_color=0
"let g:vimim_mode = 'static'
"let g:vimim_map= 'no-gi'

" Conque ---------------------------------------------- {{{2
" http://code.google.com/p/conque/
" interactive shell in vim buffer
" Bash
" nmap <C-F5> :ConqueTerm zsh<CR>
" nmap <F5> :ConqueTermSplit zsh<CR>
" Python Shell
" nmap <C-F6> :ConqueTerm python<CR>
" nmap <F6> :ConqueTermSplit python<CR>
" gdb
" nmap <C-F7> :ConqueTerm gdb<CR>
" nmap <F7> :ConqueTermSplit gdb<CR>

" snipMate -------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=2540
" http://github.com/msanders/snipmate.vim

" use my mod. version snippet
" http://code.google.com/p/8up/source/browse/#hg/utils/snipmate_ext
"
let g:snips_cstyle = "K&R"

" for c coding style
if !exists("g:snips_cstyle")
  let g:snips_cstyle = "ANSI"
endif

let g:cs = " "
let g:ce = '\n'

if g:snips_cstyle ==? "ANSI"
  let g:cs = '\n'
  let g:ce = '\n'
elseif g:snips_cstyle ==? "K&R"
  " seperate cs and ce to avoid excessive trailing spaces
  let g:cs = ' '
  let g:ce = ''
endif

"set runtimepath+=~/projects/snipmate
"set runtimepath+=~/projects/snipmate/after

" xpt, XP Templates ----------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=2611
" http://code.google.com/p/xptemplate

" use <Tab> key as trigger
" let g:xptemplate_key = '<Tab>'
" no spaces inside ()
" let g:xptemplate_vars = "SParg="

" not going to set it now.
" let g:xptemplate_vars = "author=somebody&email=nobody@gmail.com"

" set runtimepath+=~/projects/xpt
" set runtimepath+=~/projects/xpt/after

" Sparkup --------------------------------------------- {{{2
" You can write HTML in a CSS-like syntax, and have Sparkup handle the
" expansion to full HTML code.
" http://github.com/rstacruz/sparkup
" try this:
" ihtml:xxs>#wrapper>#nav>h2{navigation}+ul>li#id_$*3>a<<<#main{Page Content}+div#footer{Footer}<c-tab>
let g:sparkupExecuteMapping = '<c-tab>'
let g:sparkupNextMapping = '<tab><tab>'

" ZenCoding.vim --------------------------------------- {{{2
" vim plugins for HTML and CSS hi-speed coding.
" http://www.vim.org/scripts/script.php?script_id=2981
" http://mattn.github.com/zencoding-vim/
" This one has more features, I am not using this one right now.
" It doesn't honor my sw, sts settings.

" CtrlP
let g:ctrlp_custom_ignore = '\.neocon$\|Maildir$'
noremap <C-k> :CtrlPBuffer<CR>

" Slimv ----------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=2531
" https://bitbucket.org/kovisoft/slimv/
" let g:slimv_swank_cmd = '! xterm -e "sbcl --load ' . $HOME . '/.vim/bundle/slimv/slime/start-swank.lisp" &'

" Rainbow Parentheses --------------------------------- {{{2
" https://github.com/kien/rainbow_parentheses.vim

" Powerline
let g:Powerline_symbols = 'fancy'

" syntastic
let g:syntastic_loc_list_height=5
let g:syntastic_stl_format="Err:%fe %e,%w"
noremap <Leader><Space> :SyntasticCheck<CR>

" NeoComplCache
" Use neocomplcache.
let g:neocomplcache_enable_at_startup = 1
" Use smartcase.
let g:neocomplcache_enable_smart_case = 1
" Use camel case completion.
let g:neocomplcache_enable_camel_case_completion = 1
" Use underbar completion.
let g:neocomplcache_enable_underbar_completion = 1
" Set minimum syntax keyword length.
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'
" Define keyword.
if !exists('g:neocomplcache_keyword_patterns')
  let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'
let g:neocomplcache_snippets_dir = "~/.vim/snippets"
smap <C-k> <Plug>(neocomplcache_snippets_expand)
imap <C-k> <Plug>(neocomplcache_snippets_expand)
" Commands, Mappings and Functions ------------------------------ {{{1
" <Space> in Normal mode ------------------------------ {{{2
" Quick command mode
nnoremap <Space> :
" ErrorsToggle & QFixToggle ------------------------------------- {{{2
function! ErrorsToggle()
  if exists("g:is_error_window")
    lclose
    unlet g:is_error_window
  else
    botright lwindow 5
    let g:is_error_window = 1
  endif
endfunction

"command! -bang -nargs=? QFixToggle call QFixToggle(<bang>0)
function! QFixToggle()
  if exists("g:qfix_win")
    cclose
    unlet g:qfix_win
  else
    botright copen 5
    let g:qfix_win = 1
  endif
endfunction
nnoremap <Leader>l :call ErrorsToggle()<CR>
nnoremap <Leader>q :call QFixToggle()<CR>
" DiffOrig ------------------------------------------------------ {{{2
" Convenient command to see the difference between the current buffer and the
" file it was loaded from, thus the changes you made.
" Only define it when not defined already.
if !exists(":DiffOrig")
  command DiffOrig vert new | set bt=nofile | r ++edit # | 0d_ | diffthis
		  \ | wincmd p | diffthis
endif

" StripTrailingWhitespace ----------------------------- {{{2
function! StripTrailingWhitespace()
  " To disable this function, either set ft as keewhitespace prior saving
  " or define a buffer local variable named keepWhitespace
  if &ft =~ 'whitespace\|keep_whitespace' || exists('b:keep_whitespace')
    return
  endif
  let l:savedview = winsaveview()
  silent! %s/\s*$//e
  call winrestview(l:savedview)
endfunction

" Popup Menu in IDE style ----------------------------- {{{2
" From
" http://vim.wikia.com/wiki/Make_Vim_completion_popup_menu_work_just_like_in_an_IDE
"set completeopt=longest,menuone
"set completeopt=longest
"inoremap <expr> <CR> pumvisible() ? "\<C-y>" : "\<C-g>u\<CR>"
"inoremap <expr> <C-n> pumvisible() ? '<C-n>' :
"  \ '<C-n><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'
"inoremap <expr> <M-,> pumvisible() ? '<C-n>' :
"  \ '<C-x><C-o><C-n><C-p><C-r>=pumvisible() ? "\<lt>Down>" : ""<CR>'

" automatically chmod +x {{{2
function Lilydjwg_chmodx()
  if strpart(getline(1), 0, 2) == '#!'
    let f = expand("%:p")
    if stridx(getfperm(f), 'x') != 2
      call system("chmod +x ".shellescape(f))
      e!
      filetype detect
      nmap <buffer> <S-F5> :!%:p<CR>
    endif
  endif
endfunction

" open url with firefox {{{2
function Lilydjwg_get_pattern_at_cursor(pat)
  let col = col('.') - 1
  let line = getline('.')
  let ebeg = -1
  let cont = match(line, a:pat, 0)
  while (ebeg >= 0 || (0 <= cont) && (cont <= col))
    let contn = matchend(line, a:pat, cont)
    if (cont <= col) && (col < contn)
      let ebeg = match(line, a:pat, cont)
      let elen = contn - ebeg
      break
    else
      let cont = match(line, a:pat, contn)
    endif
  endwhile
  if ebeg >= 0
    return strpart(line, ebeg, elen)
  else
    return ""
  endif
endfunction

function Lilydjwg_open_url()
  let s:url = Lilydjwg_get_pattern_at_cursor('\v(https?://|ftp://|file:/{3}|www\.)(\w|[.-])+(:\d+)?(/(\w|[~@#$%^&+=/.?:-])+)?')
  if s:url == ""
    echohl WarningMsg
    echomsg '在光标处未发现URL！'
    echohl None
  else
    echo '打开URL：' . s:url
    if !(has("win32") || has("win64"))
      " call system("gnome-open " . s:url)
      call system("setsid firefox '" . s:url . "' &")
    else
      " start 不是程序，所以无效。并且，cmd 只能使用双引号
      " call system("start '" . s:url . "'")
      call system("cmd /q /c start \"" . s:url . "\"")
    endif
  endif
  unlet s:url
endfunction
nmap <silent> tf :call Lilydjwg_open_url()<CR>

" cscope ---------------------------------------------- {{{2
if has('cscope')
  function LoadCscopeOut()
    let parent = 0
    let dir = "."
    while parent < 5
      if filereadable(dir . "/cscope.out")
        if exists("b:cscope_prepend") && cscope_connection(3, "out", b:cscope_prepend)
          break
        end
        if cscope_connection()
          exe "cs kill " . 0
        endif
        exe "cs add " . dir . "/cscope.out" . " " . dir
        let b:cscope_prepend = dir
        break
      endif
      let parent += 1
      let dir = "../" . dir
    endwhile
  endfunc

  augroup autoload_cscope
    au!
    au BufEnter *.[ch] call LoadCscopeOut()
    au BufEnter *.hpp call LoadCscopeOut()
    au BufEnter *.cc call LoadCscopeOut()
    au BufEnter *.cpp call LoadCscopeOut()
    au BufEnter *.cxx call LoadCscopeOut()
  augroup END

  " Use both cscope and ctag
  set cscopetag
  " Show msg when cscope db added
  set cscopeverbose
  " Use tags for definition search first
  set cscopetagorder=1
  " Use quickfix window to show cscope results
  set cscopequickfix=s-,g-,d-,c-,t-,e-,f-,i-

  " Cscope mappings
  nnoremap <C-w>\ :cs find c <C-R>=expand("<cword>")<CR><CR>
  nnoremap <C-\>s :cs find s <C-R>=expand("<cword>")<CR><CR>
  nnoremap <C-\>g :cs find g <C-R>=expand("<cword>")<CR><CR>
  nnoremap <C-\>d :cs find d <C-R>=expand("<cword>")<CR><CR>
  nnoremap <C-\>c :cs find c <C-R>=expand("<cword>")<CR><CR>
  nnoremap <C-\>t :cs find t <C-R>=expand("<cword>")<CR><CR>
  nnoremap <C-\>e :cs find e <C-R>=expand("<cword>")<CR><CR>
  nnoremap <C-\>f :cs find f <C-R>=expand("<cfile>")<CR><CR>
  nnoremap <C-\>i :cs find i ^<C-R>=expand("<cfile>")<CR>$<CR>
endif
" Misc --------------------- {{{1
nnoremap <Leader>h :nohls<CR>
nnoremap <Leader>p "+p<CR>
nnoremap <Leader>P "+P<CR>
nnoremap <CR> i<CR><ESC>
inoremap <C-]> <C-x><C-]>
inoremap <C-F> <C-x><C-F>
nmap gf <C-W>gf
noremap gz :bdelete<cr>
noremap gn :bnext<cr>
noremap gb :bprev<cr>

" move in insert mode
inoremap <m-h> <left>
inoremap <m-l> <Right>
inoremap <m-j> <C-o>gj
inoremap <m-k> <C-o>gk

" Error navigation
nnoremap <m-j> :lnext<cr>zvzz
nnoremap <m-k> :lprevious<cr>zvzz
nnoremap <m-Down> :cnext<cr>zvzz
nnoremap <m-Down> :cnext<cr>zvzz
nnoremap <m-Up> :cprevious<cr>zvzz

" search for visual-mode selected text
vmap / y/<C-R>"<CR>

" tab navigation
nmap tp :tabprevious<cr>
nmap tn :tabnext<cr>
nmap to :tabnew<cr>
nmap tc :tabclose<cr>


" Edit (bang)
command! -bang E e<bang>
command! -bang Q q<bang>
command! -bang W w<bang>
command! -bang QA qa<bang>
command! -bang Qa qa<bang>
command! -bang Wa wa<bang>
command! -bang WA wa<bang>
command! -bang Wq wq<bang>
command! -bang WQ wq<bang>

" Beginning & End
noremap H ^
noremap L g_
inoremap <C-a> <esc>I
inoremap <C-e> <esc>A

" Open a Quickfix window for the last search.
nnoremap <silent> <leader>/ :execute 'vimgrep /'.@/.'/g %'<CR>:botright copen<CR>

" Save & Make
nnoremap <F5> :w<CR>:make!<CR>

" Paste toggle
set pastetoggle=<F7>

let g:haddock_browser = "firefox"
autocmd BufRead *.hs setlocal equalprg=~/bin/pp-haskell.hs
nnoremap <F10> :set wrap!<CR>
cnoremap <C-R><C-L> <C-R>=getline('.')<CR>

let g:Tex_Flavor='latex'
let g:Tex_CompileRule_pdf = 'xelatex -interaction=nonstopmode $*'

