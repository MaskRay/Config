local cmd = vim.cmd
local o = vim.o
local M = {}

vim.g.mapleader = ' '

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
o.timeoutlen = 300

if vim.g.neovide then
  vim.g.neovide_cursor_animation_length = 0
  vim.g.neovide_scroll_animation_length = 0
  vim.g.neovide_scale_factor = 1.0
  local change_scale_factor = function(delta)
    vim.g.neovide_scale_factor = vim.g.neovide_scale_factor * delta
  end
  vim.keymap.set("n", "<C-=>", function()
    change_scale_factor(1.25)
  end)
  vim.keymap.set("n", "<C-->", function()
    change_scale_factor(1 / 1.25)
  end)
end

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

    'max397574/better-escape.nvim',
    'ranjithshegde/ccls.nvim',
    'stevearc/dressing.nvim',
    -- 'neoclide/coc.nvim',
    'lewis6991/gitsigns.nvim',
    'ThePrimeagen/harpoon',
    'nvimtools/hydra.nvim',
    {'smoka7/hop.nvim', config = true},
    'rluba/jai.vim',
    'kdheepak/lazygit.nvim',
    'ggandor/leap.nvim',
    'nvimdev/lspsaga.nvim',
    'williamboman/mason.nvim',
    'williamboman/mason-lspconfig.nvim',
    'jay-babu/mason-nvim-dap.nvim',
    {'echasnovski/mini.ai', config = true},
    {'echasnovski/mini.bracketed', config = true},
    {'echasnovski/mini.sessions', config = true},
    {'echasnovski/mini.starter', config = true},
    'echasnovski/mini.surround',
    'NeogitOrg/neogit',
    {
      'nvim-neo-tree/neo-tree.nvim',
      branch = "v3.x",
      dependencies = {
        "nvim-lua/plenary.nvim",
        "nvim-tree/nvim-web-devicons",
        "MunifTanjim/nui.nvim",
      }
    },

    'alaviss/nim.nvim',
    {'hrsh7th/nvim-cmp', dependencies = {'hrsh7th/cmp-buffer', 'hrsh7th/cmp-nvim-lsp'}},
    {'folke/noice.nvim', config = true},
    'terrortylor/nvim-comment',
    'mfussenegger/nvim-dap',
    'mfussenegger/nvim-dap-python',
    {'rcarriga/nvim-dap-ui', dependencies = {'mfussenegger/nvim-dap', 'nvim-neotest/nvim-nio'}},
    'jonboh/nvim-dap-rr',
    'neovim/nvim-lspconfig',
    {'nvim-treesitter/nvim-treesitter', build = ':TSUpdate'},
    -- {'romgrk/nvim-treesitter-context', config = function() require('treesitter-context').setup() end},
    'nvim-treesitter/nvim-treesitter-textobjects',

    -- 'pwntester/octo.nvim',
    {'stevearc/overseer.nvim', opts = {}},
    'nvim-treesitter/playground',
    {
      'folke/snacks.nvim', opts = {
        statuscolumn = {enabled=false},
      }
    },
    {'nvim-telescope/telescope.nvim', dependencies = {'nvim-lua/plenary.nvim'}},
    {'nvim-telescope/telescope-fzf-native.nvim', build = 'make'},
    'aaronik/treewalker.nvim',
    'folke/trouble.nvim',
     {'akinsho/toggleterm.nvim', config = true},
    'justinmk/vim-dirvish',
    'tpope/vim-fugitive',
    'dstein64/vim-startuptime',
    'vim-crystal/vim-crystal',
    'preservim/vimux',
    'folke/which-key.nvim',
  },
  performance = {
    rtp = {
      disabled_plugins = {
        '2html_plugin',
        'tohtml',
        'getscript',
        'getscriptPlugin',
        'gzip',
        'logipat',
        'netrw',
        'netrwPlugin',
        'netrwSettings',
        'netrwFileHandlers',
        'matchit',
        'tar',
        'tarPlugin',
        'rrhelper',
        'spellfile_plugin',
        'vimball',
        'vimballPlugin',
        'zip',
        'zipPlugin',
        'tutor',
        'rplugin',
        'syntax',
        'synmenu',
        'optwin',
        'compiler',
        'bugreport',
        'ftplugin',
      },
    },
  },
})

require'tokyonight'.setup {
  style = 'night',
  on_colors = function(colors)
    colors.comment = '#939dc6'
  end,
  on_highlights = function(hl, colors)
    hl.LineNr = {fg = "#939dc6"}
  end,
}

pcall(cmd, 'colorscheme tokyonight')

-- Mappings {{{1
local function map(mode, lhs, rhs, opts)
  local options = {}
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
-- diagnostic
local diagnostic_goto = function(next, severity)
  local go = next and vim.diagnostic.goto_next or vim.diagnostic.goto_prev
  severity = severity and vim.diagnostic.severity[severity] or nil
  return function()
    go({ severity = severity })
  end
end

map({'n', 'x'}, ':', ';')
map({'n', 'x'}, ';', ':')

-- g
nmap('ga', ':<C-u>CocList -I symbols<cr>')
-- nmap('gj', ':HopLineAC<cr>', 'Hop line down')
-- nmap('gk', ':HopLineBC<cr>', 'Hop line up')
nmap('gj', ':Treewalker Down<CR>')
nmap('gk', ':Treewalker Up<CR>')
nmap('gh', ':Treewalker Left<CR>')
nmap('gl', ':Treewalker Right<CR>')
-- <leader>
nmap('<leader><space>', require('telescope.builtin').buffers, '[ ] Find existing buffers')
nmap('<leader>.', '<cmd>lua require("telescope.builtin").find_files({search_dirs={vim.fn.expand("%:h:p")}})<cr>', 'Find .')
nmap('<leader>?', function() require('dapui').eval(nil, { enter = true }) end)
-- <leader>a (app)
nmap('<leader>ag', '<cmd>%!genhdr<cr>')
nmap('<leader>aG', '<cmd>%!genhdr windows<cr>')
-- <leader>b (buffer)
nmap('<leader>be', function() require'neo-tree.command'.execute({source = 'buffers', toggle = true}) end, 'Buffer explorer')
nmap('<leader>bn', '<cmd>bn<cr>', 'Next buffer')
nmap('<leader>bp', '<cmd>bp<cr>', 'Previous buffer')
nmap('<leader>bN', '<cmd>new<cr>', 'New empty buffer')
nmap('<leader>bR', '<cmd>e<cr>')
-- <leader>d (debug, diff)
nmap('<leader>db', function() require('dap').toggle_breakpoint() end)
nmap('<leader>dc', function() require('dap').continue() end)
nmap('<leader>dC', function() require('dap').run_to_cursor() end)
nmap('<leader>de', function() require('dap').eval() end)
nmap('<leader>dl', function() require('dap').run_last() end)
nmap('<leader>do', '<cmd>bufdo diffoff<cr>')
nmap('<leader>dp', function() require('dap').pause() end)
nmap('<leader>dt', function() require('dap').terminate() end)
nmap('<leader>du', function() require('dapui').toggle() end)
nmap('<leader>dw', function() require('dap.ui.widgets').hover() end)
-- <leader>e (error)
nmap('<leader>ee', ':e <C-r>=expand("%:p:h") . "/"<cr>')
-- <leader>f (find & file)
nmap('<leader>f\'', '<cmd>Telescope marks<cr>')
nmap('<leader>f<cr>', '<cmd>Telescope resume<cr>')
nmap('<leader>fc', '<cmd>e ~/.config/nvim/init.lua<cr>')
nmap('<leader>fe', function() require'neo-tree.command'.execute({toggle = true, dir = MyProject()}) end, 'Explorer (project)')
nmap('<leader>fE', function() require'neo-tree.command'.execute({toggle = true, dir = vim.uv.cwd()}) end, 'Explorer (cwd)')
nmap('<leader>ff', '<cmd>Telescope find_files<cr>')
nmap('<leader>fg', '<cmd>Telescope git_files<cr>')
nmap('<leader>fk', function() require('which-key').show({global=false}) end, 'which-key')
nmap('<leader>fr', '<cmd>Telescope oldfiles<cr>')
-- <leader>g (git)
nmap('<leader>gb', function() require'my.util'.blame_line() end)
nmap('<leader>gB', '<cmd>Git blame<cr>')
nmap('<leader>gd', '<cmd>Git diff<cr>')
nmap('<leader>ge', function() require'neo-tree.command'.execute({source='git_status', toggle = true}) end, 'Git explorer')
nmap('<leader>gf', function() Snacks.lazygit.log_file() end, 'Lazygit Current File History')
nmap('<leader>gg', function() Snacks.lazygit( { cwd = MyProject() }) end, 'Lazygit (Root Dir)')
nmap('<leader>gG', function() Snacks.lazygit() end, 'Lazygit (cwd)')
nmap('<leader>gl', function() Snacks.lazygit.log({ cwd = MyProject() }) end, 'Lazygit Log')
nmap('<leader>gL', function() Snacks.lazygit.log() end, 'Lazygit Log (cwd)')
nmap('<leader>gw', function() require'snacks'.gitbrowse() end, 'Git Browse')
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
nmap('<leader>lr', [[:%s/\<<C-r><C-w>\>//g<Left><Left>]], 'Rename')
-- <leader>o (overseer)
nmap('<leader>ow', '<cmd>OverseerToggle<cr>')
nmap('<leader>oo', '<cmd>OverseerRun<cr>')
nmap('<leader>oq', '<cmd>OverseerQuickAction<cr>')
nmap('<leader>oi', '<cmd>OverseerInfo<cr>')
nmap('<leader>ob', '<cmd>OverseerBuild<cr>')
nmap('<leader>ot', '<cmd>OverseerTaskAction<cr>')
nmap('<leader>oc', '<cmd>OverseerClearCache<cr>')
-- <leader>p (project)
nmap('<leader>pf', '<cmd>lua require("telescope.builtin").find_files({search_dirs={MyProject()}})<cr>', {silent=true, desc='Find file in project'})
-- <leader>q (quit)
nmap('<leader>qq', '<cmd>quit<cr>')
-- <leader>s (search)
nmap('<leader>sd', '<cmd>lua require("telescope.builtin").live_grep({cwd=vim.fn.expand("%:p:h")})<cr>', 'Search directory')
nmap('<leader>sh', '<cmd>Telescope help_tags<cr>')
nmap('<leader>si', '<cmd>Telescope lsp_document_symbols<cr>')
nmap('<leader>sk', '<cmd>Telescope keymaps<cr>')
nmap('<leader>sp', '<cmd>lua require("telescope.builtin").live_grep({cwd=MyProject()})<cr>', 'Search project')
nmap('<leader>ss', '<cmd>Telescope current_buffer_fuzzy_find<cr>', 'Search buffer')
nmap('<leader>sna', function() require'noice'.cmd('all') end, 'Noice All')
nmap('<leader>snd', function() require'noice'.cmd('dismiss') end, 'Dismiss All')
nmap('<leader>snh', function() require'noice'.cmd('history') end, 'Noice History')
nmap('<leader>snl', function() require'noice'.cmd('last') end, 'Noice Last Message')
nmap('<leader>snt', function() require'noice'.cmd('pick') end, 'Noice Picker (Telescope/FzfLua)')
-- <leader>t (toggle & terminal)
nmap('<leader>tf', function() require'toggleterm'.toggle(vim.v.count, nil, MyProject(), 'float', nil) end)
nmap('<leader>th', function() require'toggleterm'.toggle(vim.v.count, 10, MyProject(), 'horizontal', nil) end)
nmap('<leader>tv', function() require'toggleterm'.toggle(vim.v.count, 80, MyProject(), 'vertical', nil) end)
nmap('<leader>tl', '<cmd>Trouble loclist toggle<cr>', 'Location List (Trouble)')
nmap('<leader>tn', '<cmd>set number!<cr>')
nmap('<leader>tq', '<cmd>Trouble qflist toggle<cr>', 'Quickfix List (Trouble)')
nmap('<leader>ts', '<cmd>set spell!<cr>')
nmap('<leader>tw', '<cmd>set wrap!<cr>')
-- u (inspect)
nmap('<leader>ui', '<cmd>Inspect<cr>')
nmap('<leader>uI', '<cmd>InspectTree<cr>')
-- w (window)
nmapp('<leader>wo', '<C-w>o')
nmapp('<leader>ws', '<C-w>s')
nmapp('<leader>wv', '<C-w>v')
-- x (trouble)
nmap('<leader>xx', '<cmd>Trouble diagnostics toggle<cr>', 'Diagnostics (Trouble)')
nmap('<leader>xX', '<cmd>Trouble diagnostics toggle filter.buf=0<cr>', 'Buffer Diagnostics (Trouble)')
nmap('<leader>cs', '<cmd>Trouble symbols toggle<cr>', 'Symbols (Trouble)')
nmap('<leader>cS', '<cmd>Trouble lsp toggle<cr>', 'LSP references/definitions/... (Trouble)')
-- misc
nmap('<M-down>', '<cmd>cnext<cr>')
nmap('<M-up>', '<cmd>cprevious<cr>')
nmap('H', '<cmd>pop<cr>', 'Tag stack backward')
nmap('J', 'gd', {remap=true})
nmap('L', '<cmd>tag<cr>', 'Tag stack forward')
nmap('U', function()
  require'hop'.hint_words()
  require'telescope.builtin'.lsp_definitions()
end, 'Hop+definition')
nmap('zx', '<cmd>bdelete<cr>')
nmap('[q', function()
  if require('trouble').is_open() then
    require('trouble').prev({ skip_groups = true, jump = true })
  else
    local ok, err = pcall(vim.cmd.cprev)
    if not ok then
      vim.notify(err, vim.log.levels.ERROR)
    end
  end
end)
nmap(']q', function()
  if require('trouble').is_open() then
    require('trouble').next({ skip_groups = true, jump = true })
  else
    local ok, err = pcall(vim.cmd.cnext)
    if not ok then
      vim.notify(err, vim.log.levels.ERROR)
    end
  end
end)
nmap('<leader>cd', vim.diagnostic.open_float, 'Line Diagnostics')
nmap(']d', diagnostic_goto(true), 'Next Diagnostic')
nmap('[d', diagnostic_goto(false), 'Prev Diagnostic')
nmap(']e', diagnostic_goto(true, 'ERROR'), 'Next Error')
nmap('[e', diagnostic_goto(false, 'ERROR'), 'Prev Error')
nmap(']w', diagnostic_goto(true, 'WARN'), 'Next Warning')
nmap('[w', diagnostic_goto(false, 'WARN'), 'Prev Warning')
nmap('<f1>', '<cmd>Gdb<cr>')
nmap('<f2>', '<cmd>Program<cr>')
nmap('<f11>', '<cmd>Break<cr>')
nmap('<f12>', '<cmd>Clear<cr>')
map({'i', 'n'}, '<C-s>', '<cmd>w<cr><esc>', 'Save file')
nmap('<C-h>', '<C-w>h')
nmap('<C-j>', '<C-w>j')
nmap('<C-k>', '<C-w>k')
nmap('<C-l>', '<C-w>l')
nmap('<M-`>', '<cmd>OverseerToggle<cr>')
nmap('<C-i>', '<C-i>')
nmap('<Tab><Tab>', '<cmd>tabnew<cr>')
nmap('<Tab>d', '<cmd>tabclose<cr>')
nmap('<Tab>f', '<cmd>tabfirst<cr>')
nmap('<Tab>l', '<cmd>tablast<cr>')
tmap('<C-h>', '<cmd>wincmd h<cr>')
tmap('<C-j>', '<cmd>wincmd j<cr>')
tmap('<C-k>', '<cmd>wincmd k<cr>')
tmap('<C-l>', '<cmd>wincmd l<cr>')
tmap('<C-s>', '<C-\\><C-n>')
map({'n', 't'}, '<C-/>', '<cmd>ToggleTerm<cr>')
map({'n', 't'}, '<C-_>', '<cmd>ToggleTerm<cr>')

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
}
nvim_create_augroups(autocmds)

-- close some filetypes with <q>
vim.api.nvim_create_autocmd("FileType", {
  group = vim.api.nvim_create_augroup("my_close_with_q", {clear=true}),
  pattern = {
    "checkhealth",
    "dbout",
    "help",
    "lspinfo",
    "neotest-output",
    "neotest-output-panel",
    "neotest-summary",
    "notify",
    "qf",
    "snacks_win",
    "spectre_panel",
    "startuptime",
    "tsplayground",
  },
  callback = function(event)
    vim.bo[event.buf].buflisted = false
    vim.schedule(function()
      vim.keymap.set("n", "q", function()
        vim.cmd("close")
        pcall(vim.api.nvim_buf_delete, event.buf, { force = true })
      end, {
        buffer = event.buf,
        silent = true,
        desc = "Quit buffer",
      })
    end)
  end,
})

--cmd 'autocmd TermOpen * startinsert'
cmd 'autocmd WinEnter * if &buftype ==# "terminal" | startinsert | endif'

function MyProject()
  return require('my.util').get_root()
end

vim.api.nvim_exec2([[
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

cmd 'filetype plugin on'
cmd 'filetype plugin indent on'

vim.filetype.add({extension = {inc = 'cpp'}})
vim.filetype.add({
  extension = {
    c3 = "c3",
    c3i = "c3",
    c3t = "c3",
  },
})

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

local function file_exists(name)
  local f = io.open(name, "r")
  return f ~= nil and io.close(f)
end
if file_exists(vim.fn.stdpath('config') .. '/lua/local.lua') then
  require 'local'
end
