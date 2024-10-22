local cmd = vim.cmd
local fn = vim.fn
local g = vim.g
local o = vim.o
local M = {}

g.mapleader = ' '

o.softtabstop = 2
o.shiftwidth = 2
o.expandtab = true
o.number = true
o.termguicolors = true
o.grepprg = 'rg --vimgrep'
o.grepformat = '%f:%l:%c:%m'

o.backup = true
o.backupdir = os.getenv('HOME') .. '/.cache/nvim/backup//'
o.directory = os.getenv('HOME') .. '/.cache/nvim/swap//'
o.undofile = true
o.undodir = os.getenv('HOME') .. '/.cache/nvim/undo//'
o.updatetime = 100

cmd 'filetype plugin on'
cmd 'filetype plugin indent on'

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

local lazypath = vim.fn.stdpath('data') .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
  vim.fn.system({'git', 'clone', '--filter=blob:none', 'https://github.com/folke/lazy.nvim.git', '--branch=stable', lazypath})
end
vim.opt.rtp:prepend(lazypath)

require('lazy').setup({
  spec = {
    'folke/tokyonight.nvim',

    'ranjithshegde/ccls.nvim',
    'stevearc/dressing.nvim',
    -- 'neoclide/coc.nvim',
    'junegunn/fzf',
    'junegunn/fzf.vim',
    'lewis6991/gitsigns.nvim',
    'ThePrimeagen/harpoon',
    'phaazon/hop.nvim',
    'rluba/jai.vim',
    'kdheepak/lazygit.nvim',
    'ggandor/lightspeed.nvim',
    'nvimdev/lspsaga.nvim',
    'williamboman/mason.nvim',
    'williamboman/mason-lspconfig.nvim',
    'NeogitOrg/neogit',
    'alaviss/nim.nvim',
    {'hrsh7th/nvim-cmp', dependencies = {'hrsh7th/cmp-buffer', 'hrsh7th/cmp-nvim-lsp'}},
    'terrortylor/nvim-comment',
    'mfussenegger/nvim-dap',
    'neovim/nvim-lspconfig',
    {'nvim-treesitter/nvim-treesitter', build = ':TSUpdate'},
    -- {'romgrk/nvim-treesitter-context', config = function() require('treesitter-context').setup() end},
    'nvim-treesitter/nvim-treesitter-textobjects',
    'pwntester/octo.nvim',
    {'stevearc/overseer.nvim', opts = {}},
    'nvim-treesitter/playground',
    {'nvim-telescope/telescope.nvim', dependencies = {'nvim-lua/plenary.nvim'}},
     {'akinsho/toggleterm.nvim', config = true},
    'justinmk/vim-dirvish',
    'tpope/vim-fugitive',
    'mhinz/vim-grepper',
    'dstein64/vim-startuptime',
    'vim-crystal/vim-crystal',
    'preservim/vimux',
    'folke/which-key.nvim',
  },
  performance = {
    rtp = {
      disabled_plugins = {
        'gzip',
        'matchit',
        'matchparen',
        'netrwPlugin',
        'tarPlugin',
        'tohtml',
        'tutor',
        'zipPlugin',
      },
    },
  },
})

pcall(cmd, 'colorscheme tokyonight')

-- Mappings {{{1
local function map(mode, lhs, rhs, opts)
  local options = {noremap = true}
  if opts then
    if type(opts) == 'string' then
      opts = {desc = opts}
    end
    options = vim.tbl_extend('force', options, opts)
  end
  vim.keymap.set(mode, lhs, rhs, options)
end
local function nmap(lhs, rhs, opts)
  map('n', lhs, rhs, opts)
end
local function tmap(lhs, rhs, opts)
  map('t', lhs, rhs, opts)
end
local function nmapp(lhs, rhs, opts)
  local options = {}
  if opts then
    if type(opts) == 'string' then
      opts = {desc = opts}
    end
    options = vim.tbl_extend('force', options, opts)
  end
  vim.api.nvim_set_keymap('n', lhs, rhs, options)
end

local function xnmap(keys, func, desc)
  vim.keymap.set('n', keys, func, { buffer = bufnr, desc = desc })
end

map('n', ':', ';')
map('n', ';', ':')
map('x', ':', ';')
map('x', ';', ':')

-- g
nmap('ga', ':<C-u>CocList -I symbols<cr>')
nmap('gj', ':HopLineAC<cr>', 'Hop line down')
nmap('gk', ':HopLineBC<cr>', 'Hop line up')
-- <leader>
xnmap('<leader>?', require('telescope.builtin').oldfiles, '[?] Find recently opened files')
xnmap('<leader><space>', require('telescope.builtin').buffers, '[ ] Find existing buffers')
nmap('<leader>.', '<cmd>lua require("telescope.builtin").find_files({search_dirs={vim.fn.expand("%:h:p")}})<cr>', 'Find .')
-- <leader>a (app)
nmap('<leader>ag', '<cmd>%!genhdr<cr>')
nmap('<leader>aG', '<cmd>%!genhdr windows<cr>')
-- <leader>b (buffer)
nmap('<leader>bn', '<cmd>bn<cr>', 'Next buffer')
nmap('<leader>bp', '<cmd>bp<cr>', 'Previous buffer')
nmap('<leader>bN', '<cmd>new<cr>', 'New empty buffer')
nmap('<leader>bR', '<cmd>e<cr>')
-- <leader>c (compile)
nmap('<leader>cc', '<cmd>OverseerRun<cr>', 'OverseerRun')
-- <leader>d (debug, diff)
nmap('<leader>db', '<cmd>Break<cr>')
nmap('<leader>dt', '<cmd>diffthis<cr>')
nmap('<leader>do', '<cmd>bufdo diffoff<cr>')
-- <leader>e (error)
nmap('<leader>ee', ':e <C-r>=expand("%:p:h") . "/"<cr>')
nmapp('<leader>en', '<Plug>(coc-diagnostic-next')
nmapp('<leader>ep', '<Plug>(coc-diagnostic-prev')
-- <leader>f (find & file)
nmap('<leader>f\'', '<cmd>Telescope marks<cr>')
nmap('<leader>f<cr>', '<cmd>Telescope resume<cr>')
nmap('<leader>fc', '<cmd>cd %:p:h<cr>')
nmap('<leader>fe', '<cmd>e ~/.config/nvim/init.lua<cr>')
nmap('<leader>ff', '<cmd>Telescope find_files<cr>')
nmap('<leader>fg', '<cmd>Telescope git_files<cr>')
nmap('<leader>fk', '<cmd>Telescope keymaps<cr>')
nmap('<leader>fr', '<cmd>Telescope oldfiles<cr>')
-- <leader>g (fugitive)
nmap('<leader>gb', '<cmd>Git blame<cr>')
nmap('<leader>gd', '<cmd>Git diff<cr>')
nmap('<leader>gg', '<cmd>lua ToggleTermCmd({cmd="lazygit",direction="float"})<cr>')
nmap('<leader>gl', '<cmd>Git log<cr>')
-- <leader>h (harpoon)
local mark = require("harpoon.mark")
local ui = require("harpoon.ui")
nmap("<leader>ha", mark.add_file)
nmap("<leader>he", ui.toggle_quick_menu)
nmap("<leader>1", function() ui.nav_file(1) end, 'Harpoon 1')
nmap("<leader>2", function() ui.nav_file(2) end, 'Harpoon 2')
nmap("<leader>3", function() ui.nav_file(3) end, 'Harpoon 3')
nmap("<leader>4", function() ui.nav_file(4) end, 'Harpoon 4')
nmap("<leader>5", function() ui.nav_file(5) end, 'Harpoon 5')
nmap("<leader>6", function() ui.nav_file(6) end, 'Harpoon 6')
-- <leader>l (lsp)
nmap('<leader>le', '<cmd>CocList diagnostics<cr>', 'LSP diagnostics')
nmapp('<leader>lf', '<Plug>(coc-fix-current)', 'Fix')
-- nmap('<leader>li', '<cmd>CocList outline<cr>', 'Outline')
nmapp('<leader>lr', '<Plug>(coc-rename)', 'Rename')
-- <leader>p (project)
nmap('<leader>pf', '<cmd>lua require("telescope.builtin").find_files({search_dirs={MyProject()}})<cr>', {silent=true, desc='Find file in project'})
-- <leader>q (quit)
nmap('<leader>qq', '<cmd>quit<cr>')
-- <leader>s (search)
nmap('<leader>sd', '<cmd>lua require("telescope.builtin").live_grep({cwd=vim.fn.expand("%:p:h")})<cr>', 'Search directory')
nmap('<leader>sp', '<cmd>lua require("telescope.builtin").live_grep({cwd=MyProject()})<cr>', 'Search project')
nmap('<leader>ss', '<cmd>Telescope current_buffer_fuzzy_find<cr>', 'Search buffer')
-- <leader>t (toggle & terminal)
nmap('<leader>tf', '<cmd>ToggleTerm direction=float<cr>')
nmap('<leader>th', '<cmd>ToggleTerm direction=horizontal size=10<cr>')
nmap('<leader>tv', '<cmd>ToggleTerm direction=vertical size=80<cr>')
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
nmap('xb', '<cmd>call CocLocations("ccls","$ccls/inheritance",{})<cr>', 'base')
nmap('xB', '<cmd>call CocLocations("ccls","$ccls/inheritance",{"levels":3})<cr>', 'base 3')
nmap('xd', '<cmd>call CocLocations("ccls","$ccls/inheritance",{"derived":v:true})<cr>', 'derive')
nmap('xD', '<cmd>call CocLocations("ccls","$ccls/inheritance",{"derived":v:true,"levels":3})<cr>', 'derive 3')
nmap('xc', '<cmd>call CocLocations("ccls","$ccls/call")<cr>', 'caller')
nmap('xC', '<cmd>call CocLocations("ccls","$ccls/call",{"callee":v:true})<cr>', 'callee')
nmap('xm', '<cmd>call CocLocations("ccls","$ccls/member")<cr>', 'member')
nmap('xn', '<cmd>CocNext<cr>', 'LSP next')
nmap('xp', '<cmd>CocPrev<cr>', 'LSP previous')
nmap('xt', '<cmd>call MarkPush()<cr>:call CocAction("jumpTypeDefinition")<cr>', 'type definition')
-- misc
nmap('<M-down>', '<cmd>cnext<cr>')
nmap('<M-up>', '<cmd>cprevious<cr>')
nmap('<M-j>', '<cmd>call MarkPush()<cr>:call CocAction("jumpDefinition")<cr>')
nmapp('<M-,>', '<Plug>(coc-references)')
nmap('H', '<cmd>call MarkPop(-1)<cr>')
nmap('L', '<cmd>call MarkPop(1)<cr>')
nmap('K', '<cmd>silent call CocAction("doHover")<cr>')
nmap('U', '<cmd>lua MyHopThenDefinition()<cr>')
nmap('zx', '<cmd>bdelete<cr>')
nmap('[l', vim.cmd.lfirst)
nmap('[l', vim.cmd.lprev)
nmap('[q', vim.cmd.cfirst)
nmap('[q', vim.cmd.cprev)
nmap(']L', vim.cmd.llast)
nmap(']Q', vim.cmd.clast)
nmap(']l', vim.cmd.lfirst)
nmap(']q', vim.cmd.cnext)
nmap('<f1>', '<cmd>Gdb<cr>')
nmap('<f2>', '<cmd>Program<cr>')
nmap('<f11>', '<cmd>Break<cr>')
nmap('<f12>', '<cmd>Clear<cr>')
nmap('<M-`>', '<cmd>OverseerToggle<cr>')
tmap('<C-h>', '<C-\\><C-n><C-w>h')
tmap('<C-j>', '<C-\\><C-n><C-w>j')
tmap('<C-k>', '<C-\\><C-n><C-w>k')
tmap('<C-l>', '<C-\\><C-n><C-w>l')

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
  toggle_search_highlighting = {
    {'InsertEnter', '*', 'setlocal nohlsearch'};
  };
  coc_show_hover = {
    -- {'CursorHold', '*', 'call My_coc_hover()'};
    -- {'CursorHoldI', '*', 'silent call CocActionAsync("showSignatureHelp")'};
  };
}
nvim_create_augroups(autocmds)

function MyHopThenDefinition()
  require'hop'.hint_words()
  vim.api.nvim_call_function('MarkPush', {})
  require'telescope.builtin'.lsp_definitions()
end

function MyProject()
  return vim.fn.systemlist("git rev-parse --show-toplevel")[1]
end

vim.api.nvim_exec2([[
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

command! GdbStart :call TermDebugSendCommand('start')
command! GdbUp :call TermDebugSendCommand('up')
command! GdbDown :call TermDebugSendCommand('down')
command! GdbQuit :call TermDebugSendCommand('quit')
]], {})

vim.g.termdebug_config = {
  wide = 1,
}
local function my_gdb_keymap(process)
  for _,buf in pairs(vim.api.nvim_list_bufs()) do
    if string.find(vim.api.nvim_buf_get_name(buf), process) then
      vim.keymap.set('n', 'c', '<cmd>Continue<cr>', {buffer=buf})
      vim.keymap.set('n', 'd', '<cmd>GdbDown<cr>', {buffer=buf})
      vim.keymap.set('n', 'f', '<cmd>Finish<cr>', {buffer=buf})
      vim.keymap.set('n', 'n', '<cmd>Over<cr>', {buffer=buf})
      vim.keymap.set('n', 'r', '<cmd>GdbStart<cr>', {buffer=buf})
      vim.keymap.set('n', 's', '<cmd>Step<cr>', {buffer=buf})
      vim.keymap.set('n', 'u', '<cmd>GdbUp<cr>', {buffer=buf})
      vim.keymap.set('n', 'v', '<cmd>call TermDebugSendCommand("info locals")<cr>', {buffer=buf})
      vim.keymap.set('n', 'w', '<cmd>call TermDebugSendCommand("where")<cr>', {buffer=buf})
      vim.keymap.set('t', ';', '<C-\\><C-n>', {buffer=buf})
    end
  end
end
vim.api.nvim_exec('packadd termdebug', true)
vim.api.nvim_create_user_command('RunGdb', function(opts)
  vim.wo.scrolloff = 999
  vim.cmd "sil! unlet g:termdebug_config['command']"
  vim.cmd 'Termdebug'
  my_gdb_keymap('/bin/gdb')
end, {})
vim.api.nvim_create_user_command('RR', function(opts)
  vim.wo.scrolloff = 999
  vim.cmd "let g:termdebug_config['command'] = ['rr', 'replay', '--']"
  vim.cmd 'Termdebug'
  my_gdb_keymap('/bin/rr')
end, {})

M.user_terminals = {}
function ToggleTermCmd(opts)
  local terms = M.user_terminals
  opts = vim.tbl_deep_extend('force', {hidden = true}, opts)
  local num = vim.v.count > 0 and vim.v.count or 1
  -- if terminal doesn't exist yet, create it
  if not terms[opts.cmd] then terms[opts.cmd] = {} end
  if not terms[opts.cmd][num] then
    if not opts.count then opts.count = vim.tbl_count(terms) * 100 + num end
    local on_exit = opts.on_exit
    opts.on_exit = function(...)
      terms[opts.cmd][num] = nil
      if on_exit then on_exit(...) end
    end
    terms[opts.cmd][num] = require("toggleterm.terminal").Terminal:new(opts)
  end
  -- toggle the terminal
  terms[opts.cmd][num]:toggle()
end

require 'plugins'
