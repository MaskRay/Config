" -*- vim: set sts=2 sw=2 et fdm=marker: -------------  vim modeline -*-

" Autocommands ---------------------------------------- {{{1
if has("autocmd")
  au FileType sass :call CSS_init()
  " Markdown ------------------------------------------ {{{2
  " Show trailing whitespaces when necessary ---------- {{{2
  " That is, most of the cases other than editing source code in Whitespace,
  " the programming language.
  "augroup show_whitespaces
    "au!
    "" Make sure this will not be cleared by colorscheme
    "autocmd ColorScheme * highlight ExtraWhitespace ctermbg=red guibg=red
    "" Highlight unwanted whitespaces
    "autocmd BufWinEnter,WinEnter,InsertLeave * call MatchUnwantedWhitespaces()
    "" In insert mode, show trailing whitespaces except when typing at the end
    "" of a line
    "autocmd InsertEnter * match ExtraWhitespace /\s\+\%#\@<!$/
    "" Show whitespaces in insert mode
    "autocmd InsertEnter * set list
    "" and turn it off when leave insert mode
    "autocmd InsertLeave * set nolist
    "" Clear highlight when lose focus
    "autocmd WinLeave * call clearmatches()

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


  " Misc ---------------------------------------------- {{{2
  augroup editing
    au!
    " Toggling between number and relativenumber when entering/leaving insert mode
    "autocmd InsertEnter * set number
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


" Plugins --------------------------------------------- {{{1
" Commands, Mappings and Functions ------------------------------ {{{1
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
