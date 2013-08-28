" -*- vim: set sts=2 sw=2 et fdm=marker: -------------  vim modeline -*-

" Status Line ----------------------------------------- {{{1
set laststatus=2

set statusline=%#ColorColumn#%2n              " buffer number
set statusline+=%*»                           " separator
set statusline+=%<                            " truncate here
set statusline+=%#DiffChange#%{PWDName()}%*   " current working directory
set statusline+=%#DiffAdd#%f%*                " path to the file in the buffer
set statusline+=%#DiffOrig#%{CurrentTag()}%*  " current tag
set statusline+=%*»                           " separator
set statusline+=%#Title#%{ReposType()}%*      " current repository type
set statusline+=%*»                           " separator
set statusline+=%#ModeMsg#%{RevisionInfo()}%* " current revision info
set statusline+=%#DiffText#%m                 " modified flag
set statusline+=%r                            " readonly flag
set statusline+=%*»                           " separator
set statusline+=%#CursorLine#(%l/%L,%c)%*»    " line no./no. of lines,col no.
set statusline+=%=«                           " right align the rest
set statusline+=%#Cursor#%02B                 " value of current char in hex
set statusline+=%*«                           " separator
set statusline+=%#ErrorMsg#%o                 " byte offset
set statusline+=%*«                           " separator
set statusline+=%#Title#%y                    " filetype
set statusline+=%*«                           " separator
set statusline+=%#ModeMsg#%3p%%               " % through file in lines
set statusline+=%*                            " restore normal highlight


" Autocommands ---------------------------------------- {{{1
if has("autocmd")
  " Markdown ------------------------------------------ {{{2
  autocmd BufNewFile,BufRead *.md setfiletype markdown
  autocmd BufNewFile,BufRead *.iced setfiletype coffee
  " Show trailing whitespaces when necessary ---------- {{{2
  " That is, most of the cases other than editing source code in Whitespace,
  " the programming language.
  augroup show_whitespaces
    au!
    " Make sure this will not be cleared by colorscheme
    autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
    " Highlight unwanted whitespaces
    autocmd BufWinEnter,WinEnter,InsertLeave * call MatchUnwantedWhitespaces()
    " In insert mode, show trailing whitespaces except when typing at the end
    " of a line
    autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
    " Show whitespaces in insert mode
    autocmd InsertEnter * set list
    " and turn it off when leave insert mode
    autocmd InsertLeave * set nolist
    " Clear highlight when lose focus
    autocmd WinLeave * call clearmatches()

  " Vala/Genis Support -------------------------------- {{{2
  " get vala.vim here:
  " https://live.gnome.org/Vala/Vim
  augroup vala_support
    au!
    autocmd BufRead *.vala set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
    autocmd BufRead *.vapi set efm=%f:%l.%c-%[%^:]%#:\ %t%[%^:]%#:\ %m
    autocmd BufRead,BufNewFile *.vala setfiletype vala
    autocmd BufRead,BufNewFile *.vapi setfiletype vala
    autocmd FileType vala setlocal cindent

    " indentation for genie: genie.vim
    " http://www.vim.org/scripts/script.php?script_id=2349
    " This will overrule the default filetype grads
    autocmd BufRead,BufNewFile *.gs setlocal filetype=genie

    autocmd FileType vala,genie setlocal formatoptions+=croql

  au FileType r let &makeprg="R <% --vanilla"
  au Filetype dot let &makeprg="dot -Tpng -O -v % ; feh %.png"

  " Ruby Support -------------------------------------- {{{2
  au FileType ruby :call Ruby_init()
  augroup ruby_support
    au!
    autocmd FileType ruby inoreab <buffer> #! #!/usr/bin/env ruby
    autocmd FileType ruby inoreab <buffer> #e # coding: utf-8
    nmap <leader>rc :Rcontroller<space><tab>
    nmap <leader>rh :Rhelper<space><tab>
    nmap <leader>rj :Rjavascript<space><tab>
    nmap <leader>rl :Rlayout<space><tab>
    nmap <leader>ro :Rlocale<space><tab>
    nmap <leader>rm :Rmodel<space><tab>
    nmap <leader>rt :Rspec<space><tab>
    nmap <leader>rk :Rtask<space><tab>
    nmap <leader>rs :Rstylesheet<space><tab>
    nmap <leader>rv :Rview<space><tab>

  " Python Support ------------------------------------ {{{2
  augroup python_support
    au!
    autocmd FileType python set omnifunc=pythoncomplete#Complete
    autocmd FileType python inoreab <buffer> #! #!/usr/bin/env python
    autocmd FileType python inoreab <buffer> #e # -*- coding: utf=8 -*-
    " Setting 'python_space_error_highlight' = 1 will only highlight mixed
    " tabs and spaces, I go as far as mark all tabs as error.
    autocmd Syntax python syn match ExtraWhitespace /\t/
    nmap <leader>p :call Flake8()<cr>

  " py.test Support ----------------------------------- {{{2
  augroup pytest
    au!
    autocmd FileType python nnoremap <buffer> <silent> <leader>tf <Esc>:Pytest function looponfail<CR>
    autocmd FileType python nnoremap <buffer> <silent> <leader>tc <Esc>:Pytest class looponfail<CR>
    autocmd FileType python nnoremap <buffer> <silent> <leader>tm <Esc>:Pytest method looponfail<CR>
    autocmd FileType python nnoremap <buffer> <silent> <leader>tt <Esc>:Pytest file looponfail<CR>
    autocmd FileType python nnoremap <buffer> <silent> <leader>tC <Esc>:Pytest clear<CR>
    autocmd FileType python nnoremap <buffer> <silent> <leader>te <Esc>:Pytest error<CR>
    autocmd FileType python nnoremap <buffer> <silent> <leader>tn <Esc>:Pytest next<CR>
    autocmd FileType python nnoremap <buffer> <silent> <leader>tp <Esc>:Pytest previous<CR>
    autocmd FileType python nnoremap <buffer> <silent> <leader>tF <Esc>:Pytest fails<CR>
    autocmd FileType python nnoremap <buffer> <silent> <leader>ts <Esc>:Pytest session<CR>

  " Mappings for reStructuredText: Section Headers ---- {{{2
  augroup restructuredtext
    au!
    " Normal Mode: Headings with overline and underline adornments
    autocmd FileType rst nnoremap <buffer> <localleader>h :call MarkReSTSessionTitle(1)<CR>
    " Normal Mode: Sessions with underline adornment
    autocmd FileType rst nnoremap <buffer> <localleader>s :call MarkReSTSessionTitle(0)<CR>
    " Insert Mode: Headings with overline and underline adornments
    autocmd FileType rst inoremap <buffer> <C-]> <C-\><C-O>:call MarkReSTSessionTitle(1)<CR>
    " Insert Mode: Headings with underline adornment
    autocmd FileType rst inoremap <buffer> <C-J> <C-\><C-O>:call MarkReSTSessionTitle(0)<CR>
    " Mapping for plugin DOT
    autocmd FileType rst nnoremap <buffer> <localleader>dot :DotOutlineTree<CR>

  " Java Support -------------------------------------- {{{2
  augroup java_suppoer
    au!
    autocmd FileType *.java setlocal omnifunc=javacomplete#Complete

  au BufWritePost *
        \ if getline(1) =~ "^#!/bin/[a-z]*sh" |
        \   exe "silent !chmod a+x <afile>" |
        \ endif

  " Default tab settings for different file types ----- {{{2
  augroup tab_settings
    au!
    " Indentation with hard tabs:
    " set 'shiftwidth' and 'tabstop' to the same amount, usually less than 8
    " for better viewing, leaving 'softtabstop' unset and 'expandtab' at
    " default value
    autocmd FileType c setlocal sw=2 ts=2 et
    autocmd FileType cpp setlocal sw=2 ts=2 et cino=g0,:0
    autocmd FileType go setlocal sw=2 ts=2
    autocmd FileType java setlocal sw=2 ts=2 et
    autocmd FileType php setlocal sw=2 ts=2
    autocmd FileType rust setlocal sw=2 ts=2
    " Indentation with spaces:
    " set 'shiftwidth' and 'softtabstop' to the same amount, usually turn on
    " 'expandtab' to avoid mixing spaces and tabs, leaving 'tabstop' at
    " default value.
    autocmd FileType asciidoc setlocal sw=2 sts=2 et
    autocmd FileType coffee setlocal sw=2 sts=2 et tw=79
    autocmd FileType css setlocal sw=4 sts=4 et
    autocmd FileType falcon setlocal sw=2 sts=2 et
    autocmd FileType haskell setlocal sw=2 sts=2 et
    autocmd FileType html setlocal sw=2 sts=2 et
    autocmd FileType htmlcheetah setlocal sw=2 sts=2 et
    autocmd FileType htmldjango setlocal sw=2 sts=2 et
    autocmd FileType javascript setlocal sw=2 sts=2 et
    autocmd FileType jade setlocal sw=2 sts=2 et
    autocmd FileType jinja setlocal sw=2 sts=2 et
    autocmd FileType jinja2 setlocal sw=2 sts=2 et
    autocmd FileType mason setlocal sw=2 sts=2 et
    autocmd FileType markdown setlocal sw=4 sts=4 et
    autocmd FileType ocaml setlocal sw=2 sts=2 et
    autocmd FileType perl setlocal sw=4 sts=4 et
    autocmd FileType rst setlocal sw=2 sts=2 et
    autocmd FileType ruby setlocal sw=2 sts=2 et
    autocmd FileType python setlocal sw=4 sts=4 et tw=72
    autocmd FileType scheme setlocal sw=2 sts=2 et
    autocmd FileType stylus setlocal sw=2 sts=2 et
    autocmd FileType vala setlocal sw=4 sts=4 et tw=78
    autocmd FileType xhtml setlocal sw=2 sts=2 et
    autocmd FileType xml setlocal sw=2 sts=2 et
    " Others with special requirements
    autocmd FileType make setlocal noet
    autocmd FileType sql setlocal et
    autocmd FileType tex setlocal sw=2 sts=2 et
    autocmd FileType text setlocal textwidth=72
    autocmd FileType verilog setlocal sw=2 sts=2 et

  " Language specific indentation --------------------- {{{2
  augroup switch_case_indentation
    au!
    " cino-:
    " line up case labels with switch:
    "     switch (a) {
    "     case 1:
    "         /* ... */
    "         break;
    "     default:
    "         break;
    "     }
    autocmd FileType c,cpp,vala setlocal cinoptions+=:0

  augroup case_block_indentation
    au!
    " cino-l
    " align with case label:
    "     switch (a) {
    "     case 1: {
    "         /* ... */
    "         break;
    "     }
    "     default:
    "         break;
    "     }
    autocmd FileType c,cpp,vala setlocal cinoptions+=l1

  augroup access_specifier_indentation
    au!
    " cino-g
    " no indentation for access specifiers
    "     class C {
    "     public:
    "         // ...
    "     protected:
    "         // ...
    "     private:
    "         // ...
    "     };
    autocmd FileType cpp setlocal cinoptions+=g0

  augroup namespace_indentation
    au!
    " cino-N
    " no indentation for namespace
    "     namespace {
    "     void function();
    "     }
    autocmd FileType cpp,vala setlocal cinoptions+=N-s

  augroup unclosed_parentheses_indentation
    au!
    " cino-(
    " line up inside unclosed parentheses
    "     if (c1 && (c2 ||
    "                c3))
    "         ;
    "     if (c1 &&
    "         (c2 || c3))
    "         ;
    autocmd FileType c,cpp,vala setlocal cinoptions+=(0

  augroup return_type_indentation
    au!
    " cino-t
    " no indentation for return type declarations
    "     int
    "     func()
    autocmd FileType c,cpp,vala setlocal cinoptions+=t0

  " Leave insert mode after 15 seconds of no input ---- {{{2
  augroup auto_escape
    au!
    " nice trick by winzo
    " http://www.reddit.com/r/vim/comments/kz84u/what_are_some_simple_yet_mindblowing_tweaks_to/c2ol6wd
    "autocmd CursorHoldI * stopinsert
    "autocmd InsertEnter * let updaterestore=&updatetime | set updatetime=15000
    "autocmd InsertLeave * let &updatetime=updaterestore

  " Misc ---------------------------------------------- {{{2
  augroup editing
    au!
    " Toggling between number and relativenumber when entering/leaving insert mode
    autocmd InsertEnter * set number
    "autocmd InsertLeave * set relativenumber
    " remove trailing whitespaces
    "autocmd BufWritePre * call StripTrailingWhitespace()

    " When editing a file, always jump to the last known cursor position.
    " Don't do it when the position is invalid or when inside an event handler
    " (happens when dropping a file on gvim).
    " Also don't do it when the mark is in the first line, that is the default
    " position when opening a file.
    autocmd BufReadPost *
      \ if line("'\"") > 1 && line("'\"") <= line("$") |
      \   exe "normal! g`\"" |
      \ endif

    " turn on spell checker for commit messages
    autocmd FileType gitcommit,hgcommit setlocal spell
    " and emails and plain text files
    autocmd FileType mail,text setlocal spell
    " except 'help' files
    autocmd BufEnter *.txt if &filetype == 'help' | setlocal nospell | endif

    " au FileType * exe('setl dictionary+='.$VIMRUNTIME.'/syntax/'.&filetype.'.vim')

  augroup END " --------------------------------------- }}}2
else
  set autoindent
endif " has("autocmd")

" Colorschemes ---------------------------------------- {{{1
" get them from: http://www.vim.org/
" also: http://code.google.com/p/vimcolorschemetest/ (recommanded)
"
" ColorScheme Table ----------------------------------- {{{2
" Name          | Background | C/C++ | HTML | CSS | Javascript | Django | Haskell | Lisp | Python
" badwolf       | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" bocau         | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" candycode     | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" clarity       | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" cloudy        | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" fruity        | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" hemisu        | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" inkpot        | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" ir_black      | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✗       | ✓    | ✓
" jellybeans    | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✗       | ✓    | ✓
" kib_darktango | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" koehler       | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" mint          | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✗       | ✓    | ✓
" molokai       | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" murphy        | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" native        | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" navajo-night  | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" neverness     | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" northsky      | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" oceanblack    | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" peppers       | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" redblack      | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✗       | ✓    | ✓
" relaxedgreen  | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" sift          | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" slate         | dark       | ✓     | ✓    | ✓   | ✓          | ✗      | ✓       | ✓    | ✓
" pablo         | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" twilight      | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✗       | ✓    | ✓
" vj            | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" vividchalk    | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" wintersday    | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" wombat        | dark       | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" solarized     | light/dark | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓
" kib_plastic   | light      | ✓     | ✓    | ✓   | ✓          | ✓      | ✓       | ✓    | ✓

" hemisu:
" only works in gVim
"
" ir_black:
" http://blog.infinitered.com/entries/show/8
" http://blog.infinitered.com/entry_files/8/ir_black.vim
"
" solarized:
" http://ethanschoonover.com/solarized
"
" badwolf:
" http://stevelosh.com/projects/badwolf/
" https://bitbucket.org/sjl/badwolf/
" hg clone https://bitbucket.org/sjl/badwolf
"
" }}}2

if has("gui_running")
  set background=dark
  "colorscheme molokai
  "hi Normal       guifg=White guibg=Black
  "colorscheme badwolf
  "colorscheme harlequin
  colorscheme hemisu
else
  set t_Co=256
  set background=dark
  colorscheme bocau
endif

" Plugins --------------------------------------------- {{{1
" EasyMotion ------------------------------------------ {{{2
let g:EasyMotion_do_mapping = 1
let g:EasyMotion_leader_key = ","
" Ctrl-P ---------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=3736
" http://kien.github.com/ctrlp.vim/
" https://bitbucket.org/kien/ctrlp.vim
" git clone git://github.com/kien/ctrlp.vim.git
"
" Mappings:
" let g:ctrlp_map = '<C-P>'
" Search from project root:
" let g:ctrlp_working_path_mode = 2
" Exclude VCS cache dir, either:
" set wildignore+=*/.git/*,*/.hg/*,*/.svn/*   " for Linux/MacOSX
" or:
let g:ctrlp_custom_ignore = {
  \ 'dir':  '\.git$\|\.hg$\|\.svn$\|\.neocom$\|Maildir$',
  \ 'file': '\.pyc$\|\.so$\|\.o$',
  \ }
" Match window, top of the screen:
" let g:ctrlp_match_window_bottom = 0
" switching between buffers
" nnoremap <C-k> :CtrlPBuffer<CR>
" works in gvim and some terminals.
" nnoremap <C-A-P> :CtrlPMixed<CR>

" Cute Python ----------------------------------------- {{{2
" https://github.com/ehamberg/vim-cute-python
" #git clone git://github.com/ehamberg/vim-cute-python.git

" dot.vim ------------------------------------------- {{{2
" https://bitbucket.org/shu/dotoutlinetree
" http://www.vim.org/scripts/script.php?script_id=1225

" dwm --- {{{2
nnoremap <SID>I_won’t_ever_type_this <Plug>IMAP_JumpForward

" FuzzyFinder ----------------------------------------- {{{2
" vim-l9 is the requirement of fuzzyfinder 4.*
" http://www.vim.org/scripts/script.php?script_id=3252
" http://bitbucket.org/ns9tks/vim-l9/
" #hg clone https://bitbucket.org/ns9tks/vim-l9
"
" http://www.vim.org/scripts/script.php?script_id=1984
" http://bitbucket.org/ns9tks/vim-fuzzyfinder/
" #hg clone https://bitbucket.org/ns9tks/vim-fuzzyfinder
"
" mapping for FuzzyFinder
" use V 3.4
"nnoremap <leader>ff :FufFile ~/projects/<CR>
" search from cwd
nnoremap <leader>ff :FufFile<CR>
nnoremap <leader>fb :FufBuffer<CR>
nnoremap <leader>fc :FufChangeLis[t<CR>

" Global ---------------------------------------------- {{{2
nnoremap <C-\>s :Gtags <C-r><C-w><cr>
nnoremap <C-\>r :Gtags -r <C-r><C-w><cr>
nnoremap <C-\>p :Gtags -P <C-r><C-w><cr>
nnoremap <C-\><C-\> :Gtags

" Gundo ----------------------------------------------- {{{2
" http://sjl.bitbucket.org/gundo.vim/
" hg clone https://bitbucket.org/sjl/gundo.vim
nnoremap <leader>u :GundoToggle<CR>

" Haskell Conceal ------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=3200
" https://github.com/Twinside/vim-haskellConceal
" #git clone git://github.com/Twinside/vim-haskellConceal.git

" Indent Guides --------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=3361
" https://github.com/nathanaelkane/vim-indent-guides
" git clone git://github.com/vim-scripts/Indent-Guides.git

" NERDTree -------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=1658
" https://github.com/scrooloose/nerdtree
" git clone git://github.com/scrooloose/nerdtree.git
nnoremap <leader>nt :NERDTreeToggle<CR>

" Notes ----------------------------------------------- {{{2
let g:notes_directory = '~/org'
let g:notes_suffix = '.md'
let g:notes_title_sync = 'no'

" Pathogen -------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=2332
" https://github.com/tpope/vim-pathogen
" git clone git://github.com/tpope/vim-pathogen.git

" PinyinSearch --- {{{2
let g:PinyinSearch_Dict = '/home/ray/.vim/bundle/vim-PinyinSearch/PinyinSearch.dict'
nnoremap <Leader>ps :call PinyinSearch()<CR>

" py.test --------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=3424
" https://github.com/alfredodeza/pytest.vim.git
" git clone git://github.com/alfredodeza/pytest.vim.git

" Rainbow Parentheses --------------------------------- {{{2
" https://github.com/kien/rainbow_parentheses.vim
" #git clone git://github.com/kien/rainbow_parentheses.vim.git

" Sessionman ------------------------------------------ {{{2
set sessionoptions=blank,buffers,curdir,folds,tabpages,winsize
nmap <leader>sl :SessionList<CR>
nmap <leader>ss :SessionSave<CR>

" Splice ---------------------------------------------- {{{2
" https://bitbucket.org/sjl/splice.vim
" https://github.com/sjl/splice.vim
" hg clone https://bitbucket.org/sjl/splice.vim

" Slimv ----------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=2531
" https://bitbucket.org/kovisoft/slimv/
" hg clone https://bitbucket.org/kovisoft/slimv
let g:slimv_swank_cmd = '! urxvtc -e sbcl --load ' . $HOME . '/.vim/bundle/slimv/slime/start-ecl.lisp &'

" snipMate -------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=2540
" http://github.com/msanders/snipmate.vim
" #git clone git://github.com/msanders/snipmate.vim.git

" Sparkup --------------------------------------------- {{{2
" You can write HTML in a CSS-like syntax, and have Sparkup handle the
" expansion to full HTML code.
" http://github.com/rstacruz/sparkup
" git clone git://github.com/rstacruz/sparkup.git
"
" try this:
" ihtml:xxs>#wrapper>#nav>h2{navigation}+ul>li#id_$*3>a<<<#main{Page Content}+div#footer{Footer}<c-tab>
"let g:sparkupExecuteMapping = '<C-]>'
"let g:sparkupNextMapping = '<C-m>'

" SuperTab -------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=1643
" https://github.com/ervandew/supertab
" #git clone git://github.com/ervandew/supertab.git

" Surround -------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=1697
" https://github.com/tpope/vim-surround
" git clone git://github.com/tpope/vim-surround.git

" Syntastic ------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=2736
" https://github.com/scrooloose/syntastic
" git clone git://github.com/scrooloose/syntastic.git
let g:syntastic_loc_list_height=5
let g:syntastic_stl_format="Err:%fe %e,%w"
let g:syntastic_cpp_compiler = 'clang++'
let g:syntastic_cpp_compiler_options = '-std=c++11'
nnoremap <leader>st :SyntasticToggleMode<CR>

" Tabular --------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=3464
" https://github.com/godlygeek/tabular
" git clone git://github.com/godlygeek/tabular.git
if exists(":Tabularize")
  nmap <Leader>a= :Tabularize /=<CR>
  vmap <Leader>a= :Tabularize /=<CR>
  nmap <Leader>a: :Tabularize /:<CR>
  vmap <Leader>a: :Tabularize /:<CR>
  nmap <Leader>a:: :Tabularize /:\zs<CR>
  vmap <Leader>a:: :Tabularize /:\zs<CR>
  nmap <Leader>a, :Tabularize /,<CR>
  vmap <Leader>a, :Tabularize /,<CR>
  nmap <Leader>a<Bar> :Tabularize /<Bar><CR>
  vmap <Leader>a<Bar> :Tabularize /<Bar><CR>
endif

" Tagbar ---------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=3465
" http://github.com/majutsushi/tagbar
" git clone git://github.com/majutsushi/tagbar.git
nnoremap <leader>tb :TagbarToggle<CR>
"let g:tagbar_autoclose = 1
let g:tagbar_autofocus = 1
let g:tagbar_autoshowtag = 1

let g:tagbar_type_vala = {
  \ 'ctagstype': 'c#',
  \ 'kinds': [
    \ 'c:class',
    \ 'd:macro',
    \ 'E:event',
    \ 'g:enum',
    \ 'i:interface',
    \ 'm:method',
    \ 'n:namespace',
    \ 'p:properties',
    \ 's:struct',
  \ ]
\ }

" Taglist --------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=273
" http://vim-taglist.sourceforge.net/
"nnoremap <leader>tl :TlistToggle<CR>

" TaskList -------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=2607
nnoremap <leader>tl :TaskList<CR>

" UltiSnips ------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=2715
" official mirror: https://github.com/sirver/ultisnips
" git clone git://github.com/vim-scripts/UltiSnips.git
"
" TextMate style:
let g:UltiSnipsExpandTrigger = "<Tab>"
let g:UltiSnipsJumpForwardTrigger = "<Tab>"
let g:UltiSnipsJumpBackwardTrigger = "<S-Tab>"
" local snippets only:
let g:UltiSnipsSnippetDirectories = ["snippets"]
" vsplit the snippets edit window
let g:UltiSnipsEditSplit = 'vertical'

" Vim CSS Color --------------------------------------- {{{2
" https://github.com/skammer/vim-css-color
" #git clone git://github.com/skammer/vim-css-color.git
"
" ap's clone, this one has way shorter startup time.
" https://github.com/ap/vim-css-color
" git clone git://github.com/ap/vim-css-color.git
"
" let g:cssColorVimDoNotMessMyUpdatetime = 1

" vim-coffee-script ----------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=3590
" https://github.com/kchmck/vim-coffee-script
" git clone git://github.com/kchmck/vim-coffee-script.git

" vim-commentary -------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=3695
" https://github.com/tpope/vim-commentary
" git clone git://github.com/tpope/vim-commentary.git

" vim-indent-object ----------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=3037
" https://github.com/michaeljsmith/vim-indent-object
" git clone git://github.com/michaeljsmith/vim-indent-object.git

" VimIm ----------------------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=2506
" http://code.google.com/p/vimim/
" https://github.com/vimim/vimim
" #svn checkout https://vimim.googlecode.com/svn/trunk/ vimim
" git clone git://github.com/vimim/vimim.git
"
" vimim settings, show menu background color
let g:vimim_cloud = -1
"let g:vimim_map = 'c-bslash'
"let g:vimim_mode = 'static'
"let g:vimim_mycloud = 0
let g:vimim_toggle = 'pinyin'

" xpt, XP Templates ----------------------------------- {{{2
" http://www.vim.org/scripts/script.php?script_id=2611
" http://code.google.com/p/xptemplate
" https://github.com/drmingdrmer/xptemplate
" #git clone git://github.com/drmingdrmer/xptemplate.git
"
" use <Tab> key as trigger
" let g:xptemplate_key = '<Tab>'
" no spaces inside ()
" let g:xptemplate_vars = "SParg="
"
" not going to set it now.
" let g:xptemplate_vars = "author=somebody&email=nobody@gmail.com"

" YankRing --------------------------------------------- {{{2
nmap <Leader>ys :YRShow<CR>
nmap <Leader>ye :YRGetElem<space>

" ZenCoding.vim --------------------------------------- {{{2
" vim plugins for HTML and CSS hi-speed coding.
" http://www.vim.org/scripts/script.php?script_id=2981
" http://mattn.github.com/zencoding-vim/
" This one has more features, I am not using this one right now.
" It doesn't honor my sw, sts settings.

" cd here --------------------------------------------- {{{2
" Change into directory of the current file
cnoreab cdh cd %:p:h
cnoreab lcdh lcd %:p:h

" NeoComplCache
let g:neocomplcache_enable_at_startup = 1
let g:neocomplcache_enable_smart_case = 1
let g:neocomplcache_enable_camel_case_completion = 1
let g:neocomplcache_enable_underbar_completion = 1
let g:neocomplcache_enable_auto_delimiter = 1
let g:neocomplcache_min_syntax_length = 3
let g:neocomplcache_lock_buffer_name_pattern = '\*ku\*'
" Define keyword.
if !exists('g:neocomplcache_keyword_patterns')
  let g:neocomplcache_keyword_patterns = {}
endif
let g:neocomplcache_keyword_patterns['default'] = '\h\w*'
"let g:neocomplcache_include_paths['cpp']= "/usr/include/gtkmm-3.0,/usr/include/cairomm-1.0"
"let g:neocomplcache_snippets_dir = "~/.vim/snippets"
"smap <C-k> <Plug>(neocomplcache_snippets_expand)
"imap <C-k> <Plug>(neocomplcache_snippets_expand)

" Gentoo Syntax --------------------------------- {{{2
" FIXME Get rid of nmap <Leader>bug
let g:loaded_bugsummary=1

" Fugitive --- {{{2
nnoremap <silent> <leader>gs :Gstatus<CR>
nnoremap <silent> <leader>gd :Gdiff<CR>
nnoremap <silent> <leader>gc :Gcommit<CR>
nnoremap <silent> <leader>gb :Gblame<CR>
nnoremap <silent> <leader>gl :Glog<CR>
nnoremap <silent> <leader>gp :Git push<CR>
" Unite --- {{{2
nnoremap <silent> sr :Unite -buffer-name=file -start-insert file_mru<cr>
nnoremap <silent> sf :Unite -buffer-name=file -start-insert file<cr>
nnoremap <silent> ss :Unite -buffer-name=buffer -start-insert buffer<cr>
"nnoremap <silent> sf :FufBuffer<cr>
nnoremap <silent> sc :Unite -buffer-name=change change<cr>
" Cumino --- {{{2
let g:cumino_buffer_location = "/tmp/.cumini.buff"
" Commands, Mappings and Functions ------------------------------ {{{1
" <Space> in Normal mode ------------------------------ {{{2
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

" More vala/genie support ----------------------------- {{{2
" Vala settings as described here:
" https://live.gnome.org/Vala/Vim

" Disable valadoc syntax highlight
"let vala_ignore_valadoc = 1

" Enable comment strings
let vala_comment_strings = 1

" Highlight space errors
let vala_space_errors = 1
" Disable trailing space errors
"let vala_no_trail_space_error = 1
" Disable space-tab-space errors
let vala_no_tab_space_error = 1

" Minimum lines used for comment syncing (default 50)
"let vala_minlines = 120

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


" MatchUnwantedWhitespaces ---------------------------- {{{2
function! MatchUnwantedWhitespaces()
  highlight ExtraWhitespace ctermbg=red guibg=red
  " Show all trailing whitespaces: /\s\+$/
  " and spaces followed by tabs:   / \+\t\+\s*/
  " and tabs followed by spaces:   /\t\+ \+\s*/
  " combine them together: /\s\+$\| \+\t\+\s*\|\t\+ \+\s*/
  match ExtraWhitespace /\s\+$\| \+\t\+\s*\|\t\+ \+\s*/
endfunction

" CurrentTag ------------------------------------------ {{{2
function! CurrentTag()
  if exists('b:show_tag_in_statusline') && b:show_tag_in_statusline == 1
    return tagbar#currenttag('[%s]','','f')
  else
    return ''
  endif
endfunction

" ToggleCurrentTag ------------------------------------ {{{2
function! ToggleCurrentTag()
  if exists('b:show_tag_in_statusline')
    let b:show_tag_in_statusline = !b:show_tag_in_statusline
  else
    let b:show_tag_in_statusline = 1
  endif
endfunction
command! -nargs=0 CurrentTagToggle call ToggleCurrentTag()
nnoremap <leader>ct :CurrentTagToggle<CR>

" PWDName --------------------------------------------- {{{2
function! PWDName()
  let l:pwd_name = fnamemodify(getcwd(), ':~:.')
  if l:pwd_name != ""
    let l:pwd_name .= '/'
  endif
  return l:pwd_name
endfunction

" RevisionInfo ---------------------------------------- {{{2
" Return revision info of current file
function! RevisionInfo()
  if !exists('b:revision_info')
    return ''
  endif
  return b:revision_info
endfunction

" ReposType ------------------------------------------- {{{2
" Return repository type if current file is inside one
function! ReposType()
  if !exists('b:repos_type')
    return ''
  endif
  return b:repos_type
endfunction

" UpdateRevisionInfo ---------------------------------- {{{2
" Update revision info of current file
function! UpdateRevisionInfo()
  let b:revision_info = ""
  let b:repos_type = ""

  if glob("%") == ""
    " No existing file is loaded.
    return
  endif

  " lookup path, starts from directory of current file
  " searching up upward, until file system root.
  let l:cur_dir_and_up = expand("%:p:h") . ';'
  let l:repos_info_cmd = ""

  " Is this inside a mercurial repository?
  let l:root = finddir('.hg', l:cur_dir_and_up)
  if l:root != ""
    " this is an hg repos.
    let b:repos_type = "Mercurial"
    if !exists("g:hg_id_flag")
      let g:hg_id_flag = '-Bbint'
    endif
    let l:repos_info_cmd = "hg id " . g:hg_id_flag
  else
    let l:root = finddir('.git', l:cur_dir_and_up)
    if l:root != ""
      "  git repository
      let b:repos_type = "Git"
      let l:repos_info_cmd = "git branch"
    endif
  endif
  " inside repository
  if l:repos_info_cmd != ""
    " root of repository
    let l:repos_root = fnamemodify(l:root, ":p:h")
    " try to get revision info
    let l:info = system("cd " . l:repos_root . " && " . l:repos_info_cmd)
    if v:shell_error == 0
      " with return code 0, assuming nothing went wrong
      if b:repos_type ==# "Git"
        " git does not provide enough customization of output as we need
        " remove first 2 chars, e.g, the '* ' part of '* master'
        let l:info = strpart(l:info, 2)
      endif
      let b:revision_info = substitute(l:info, '\n.*', '', 'g')
    endif
  endif
endfunction

" makes * and # work on visual mode too.
function! s:VSetSearch(cmdtype)
  let temp = @s
  norm! gv"sy
  let @/ = '\V' . substitute(escape(@s, a:cmdtype.'\'), '\n', '\\n', 'g')
  let @s = temp
endfunction

xnoremap * :<C-u>call <SID>VSetSearch('/')<CR>/<C-R>=@/<CR><CR>
xnoremap # :<C-u>call <SID>VSetSearch('?')<CR>?<C-R>=@/<CR><CR>

" recursively vimgrep for word under cursor or selection if you hit leader-star
nmap <leader>* :execute 'noautocmd vimgrep /\V' . substitute(escape(expand("<cword>"), '\'), '\n', '\\n', 'g') . '/ **'<CR>
vmap <leader>* :<C-u>call <SID>VSetSearch()<CR>:execute 'noautocmd vimgrep /' . @/ . '/ **'<CR>
