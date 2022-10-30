local cmd = vim.cmd
local fn = vim.fn
local g = vim.g
local o = vim.o

g.mapleader = ' '

o.softtabstop = 2
o.shiftwidth = 2
o.expandtab = true
o.number = true
o.termguicolors = true

o.backup = true
o.backupdir = os.getenv('HOME') .. '/.cache/nvim/backup//'
o.directory = os.getenv('HOME') .. '/.cache/nvim/swap//'
o.undofile = true
o.undodir = os.getenv('HOME') .. '/.cache/nvim/undo//'
o.updatetime = 100

cmd 'filetype plugin on'
cmd 'filetype plugin indent on'
pcall(cmd, 'colorscheme tokyonight')

local stl = {
  '%#ColorColumn#%2f',          -- buffer number
  ' ',                          -- separator
  '%<',                         -- truncate here
  '%*»',                        -- separator
  '%*»',                        -- separator
  '%#DiffText#%m',              -- modified flag
  '%r',                         -- readonly flag
  '%*»',                        -- separator
  '%#CursorLine#(%l/%L,%c)%*»', -- line no./no. of lines,col no.
  '%=«',                        -- right align the rest
  '%#Cursor#%02B',              -- value of current char in hex
  '%*«',                        -- separator
  '%#ErrorMsg#%o',              -- byte offset
  '%*«',                        -- separator
  '%#Title#%y',                 -- filetype
  '%*«',                        -- separator
  '%#ModeMsg#%3p%%',            -- % through file in lines
  '%*',                         -- restore normal highlight
}
o.statusline = table.concat(stl)

-- Mappings {{{1
local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap(mode, lhs, rhs, options)
end
local function nmap(lhs, rhs, opts)
  map('n', lhs, rhs, opts)
end
local function nmapp(lhs, rhs, opts)
  local options = {}
  if opts then options = vim.tbl_extend('force', options, opts) end
  vim.api.nvim_set_keymap('n', lhs, rhs, options)
end

map('n', ':', ';')
map('n', ';', ':')
map('x', ':', ';')
map('x', ';', ':')

-- g
nmap('ga', ':<C-u>CocList -I symbols<cr>')
nmap('gj', ':HopLineAC<cr>')
nmap('gk', ':opLineBC<cr>')
-- <leader>
nmap('<leader><leader>', '<cmd>Telescope buffers<cr>')
nmap('<leader>.', '<cmd>lua require("telescope.builtin").find_files({search_dirs={vim.fn.expand("%:h:p")}})<cr>', {silent=true})
-- <leader>a (app)
nmap('<leader>ag', '<cmd>%!genhdr<cr>')
nmap('<leader>aG', '<cmd>%!genhdr windows<cr>')
-- <leader>b (buffer)
nmap('<leader>bn', '<cmd>bn<cr>')
nmap('<leader>bp', '<cmd>bp<cr>')
nmap('<leader>bN', '<cmd>new<cr>')
nmap('<leader>bR', '<cmd>e<cr>')
-- <leader>d (diff)
nmap('<leader>dt', '<cmd>diffthis<cr>')
nmap('<leader>do', '<cmd>bufdo diffoff<cr>')
-- <leader>e (error)
nmap('<leader>ee', ':e <C-r>=expand("%:p:h") . "/"<cr>')
nmap('<leader>es', ':sp <C-r>=expand("%:p:h") . "/"<cr>')
nmap('<leader>ev', ':vsp <C-r>=expand("%:p:h") . "/"<cr>')
nmapp('<leader>en', '<Plug>(coc-diagnostic-next')
nmapp('<leader>ep', '<Plug>(coc-diagnostic-prev')
-- <leader>f (file)
nmap('<leader>fc', '<cmd>cd %:p:h<cr>')
nmap('<leader>fe', '<cmd>e ~/.config/nvim/init.lua<cr>')
nmap('<leader>ff', '<cmd>Telescope find_files<cr>')
nmap('<leader>fr', '<cmd>Telescope oldfiles<cr>')
-- <leader>g (fugitive)
nmap('<leader>gb', '<cmd>Git blame<cr>')
nmap('<leader>gd', '<cmd>Git diff<cr>')
nmap('<leader>gl', '<cmd>Git log<cr>')
nmap('<leader>gg', '<cmd>Git status<cr>')
-- <leader>l (lsp)
nmap('<leader>le', '<cmd>CocList diagnostics<cr>')
nmapp('<leader>lf', '<Plug>(coc-fix-current)')
nmap('<leader>li', '<cmd>CocList outline<cr>')
nmapp('<leader>lr', '<Plug>(coc-rename)')
-- <leader>q (quit)
nmap('<leader>qq', '<cmd>quit<cr>')
-- <leader>s (search)
nmap('<leader>sd', '<cmd>Telescope live_grep<cr>')
nmap('<leader>ss', '<cmd>lua require("telescope.builtin").grep_string()<cr>')
nmap('<leader>sS', '<cmd>Grepper -noprompt -cword<cr>')
-- <leader>t (toggle)
nmap('<leader>tl', '<cmd>lua require("fn").toggle_loclist()<cr>')
nmap('<leader>tn', '<cmd>set number!<cr>')
nmap('<leader>tq', '<cmd>lua require("fn").toggle_qf()<cr>')
nmap('<leader>ts', '<cmd>set spell!<cr>')
nmap('<leader>tw', '<cmd>set wrap!<cr>')
-- w (window)
nmapp('<leader>wo', '<C-w>o')
nmapp('<leader>ws', '<C-w>s')
nmapp('<leader>wv', '<C-w>v')
--- , (references)
nmap(',f', '<cmd>call CocLocations("ccls","textDocument/references",{"excludeRole":32})<cr>') -- not call
nmap(',m', '<cmd>call CocLocations("ccls","textDocument/references",{"role":64})<cr>') -- macro
nmap(',r', '<cmd>call CocLocations("ccls","textDocument/references",{"role":8})<cr>') -- read
nmap(',w', '<cmd>call CocLocations("ccls","textDocument/references",{"role":16})<cr>') -- write
-- x (xref)
-- bases of up to 3 levels
nmap('xb', '<cmd>call CocLocations("ccls","$ccls/inheritance",{"levels":3})<cr>')
-- derived of up to 3 levels
nmap('xd', '<cmd>call CocLocations("ccls","$ccls/inheritance",{"derived":v:true})<cr>')
-- derived of up to 3 levels
nmap('xD', '<cmd>call CocLocations("ccls","$ccls/inheritance",{"derived":v:true,"levels":3})<cr>')
-- caller
nmap('xc', '<cmd>call CocLocations("ccls","$ccls/call")<cr>')
-- callee
nmap('xC', '<cmd>call CocLocations("ccls","$ccls/call",{"callee":v:true})<cr>')
nmap('xt', '<cmd>call MarkPush()<cr>:call CocAction("jumpTypeDefinition")<cr>')
-- misc
nmap('<M-down>', '<cmd>cnext<cr>')
nmap('<M-up>', '<cmd>cprevious<cr>')
nmap('<M-j>', '<cmd>call MarkPush()<cr>:call CocAction("jumpDefinition")<cr>')
nmapp('<M-,>', '<Plug>(coc-references)')
nmap('H', '<cmd>call MarkPop(-1)<cr>')
nmap('L', '<cmd>call MarkPop(1)<cr>')
nmap('K', '<cmd>silent call CocAction("doHover")<cr>')
nmap('U', '<cmd>call MyHopThenDefinition()<cr>')
nmap('zx', '<cmd>bdelete<cr>')

-- Autocmd {{{1
function nvim_create_augroups(definitions)
  for group_name, definition in pairs(definitions) do
    vim.api.nvim_command('augroup '..group_name)
    vim.api.nvim_command('autocmd!')
    for _, def in ipairs(definition) do
      local command = table.concat(vim.tbl_flatten{'autocmd', def}, ' ')
      vim.api.nvim_command(command)
    end
    vim.api.nvim_command('augroup END')
  end
end
local autocmds = {
  restore_cursor = {
    {'BufRead', '*', [[call setpos(".", getpos("'\""))]]};
  };
  packer = {
    {'BufWritePost', 'plugins.lua', 'PackerCompile'};
  };
  toggle_search_highlighting = {
    {'InsertEnter', '*', 'setlocal nohlsearch'};
  };
  coc_show_hover = {
    -- {'CursorHold', '*', 'call My_coc_hover()'};
    -- {'CursorHoldI', '*', 'silent call CocActionAsync("showSignatureHelp")'};
  };
}
nvim_create_augroups(autocmds)

vim.api.nvim_exec([[
let g:mark_ring = [{},{},{},{},{},{},{},{},{},{}]
let g:mark_ring_i = 0

fu! MarkPush()
  let g:mark_ring[g:mark_ring_i] = {'path': expand('%:p'), 'line': line('.'), 'col': col('.')}
  let g:mark_ring_i = (g:mark_ring_i + 1) % len(g:mark_ring)
endf

fu! MarkPop(d)
  let g:mark_ring[g:mark_ring_i] = {'path': expand('%:p'), 'line': line('.'), 'col': col('.')}
  let g:mark_ring_i = (g:mark_ring_i + a:d + len(g:mark_ring)) %  len(g:mark_ring)
  let mark = g:mark_ring[g:mark_ring_i]
  if !has_key(mark, 'path')
    echo 'empty g:mark_ping'
    return
  endif
  if mark.path !=# expand('%:p')
    silent exec 'e ' . fnameescape(mark.path)
  endif
  call cursor(mark.line, mark.col)
endf

fu! MyHopThenDefinition()
  lua require'hop'.hint_words()
  call MarkPush()
  call CocAction("jumpDefinition")
endf

fu! My_coc_hover()
  if (coc#float#has_float() == 0 && CocHasProvider('hover') == 1)
    silent call CocActionAsync('doHover')
  endif
endf

fu! C_init()
  setl cino+=g0,:0,j1,l1,N-s,t0,(0
endf
aug C
  au!
  au FileType c,cpp :call C_init()
aug END

fu! Nim_init()
  setl nofoldenable
endf
aug Nim
  au!
  au FileType nim :call Nim_init()
aug END
]], true)

function prequire(...)
  local status, lib = pcall(require, ...)
  if status then return lib end
  return nil
end

cmd 'packadd packer.nvim'
local packer = prequire('packer')
if packer then
  require('packer').startup(function()
    use 'wbthomason/packer.nvim'

    use 'folke/tokyonight.nvim'

    use 'neoclide/coc.nvim'
    use 'junegunn/fzf'
    use 'junegunn/fzf.vim'
    use 'lewis6991/gitsigns.nvim'
    use 'phaazon/hop.nvim'
    use 'kdheepak/lazygit.nvim'
    use 'ggandor/lightspeed.nvim'
    use 'alaviss/nim.nvim'
    use {'hrsh7th/nvim-cmp', requires = {"hrsh7th/cmp-buffer", "hrsh7th/cmp-nvim-lsp"}}
    use 'terrortylor/nvim-comment'
    use 'mfussenegger/nvim-dap'
    use 'neovim/nvim-lspconfig'
    use 'nvim-treesitter/nvim-treesitter'
    use {'romgrk/nvim-treesitter-context', config = function() require('treesitter-context').setup() end}
    use 'nvim-treesitter/nvim-treesitter-textobjects'
    use 'nvim-treesitter/playground'
    use {'nvim-telescope/telescope.nvim', requires = {{'nvim-lua/plenary.nvim'}}}
    use 'justinmk/vim-dirvish'
    use 'tpope/vim-fugitive'
    use 'mhinz/vim-grepper'
    use 'preservim/vimux'
    use 'folke/which-key.nvim'
  end)

  require 'plugins'
end
