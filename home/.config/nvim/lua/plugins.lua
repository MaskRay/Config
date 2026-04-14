local M = {}
M.lsp = require'lsp'

require'better_escape'.setup {
  default_mappings = false,
  mappings = {
    i = {
      --  first_key[s]
      j = {
        --  second_key[s]
        k = "<Esc>",
      },
    },
    c = {
      j = {
        k = "<Esc>",
        j = "<Esc>",
      },
    },
    t = {
      j = {
        k = "<C-\\><C-n>",
      },
    },
    v = {
      j = {
        k = "<Esc>",
      },
    },
    s = {
      j = {
        k = "<Esc>",
      },
    },
  },
}

vim.keymap.set({'n', 'x', 'o'}, 's',  '<Plug>(leap-forward)')
vim.keymap.set({'n', 'x', 'o'}, 'S',  '<Plug>(leap-backward)')
vim.keymap.set('n',             'gs', '<Plug>(leap-from-window)')
vim.keymap.set({'x', 'o'},      'x',  '<Plug>(leap-forward-till)')
vim.keymap.set({'x', 'o'},      'X',  '<Plug>(leap-backward-till)')
require'mini.surround'.setup({
  mappings = {
    add = "gza", -- Add surrounding in Normal and Visual modes
    delete = "gzd", -- Delete surrounding
    find = "gzf", -- Find surrounding (to the right)
    find_left = "gzF", -- Find surrounding (to the left)
    highlight = "gzh", -- Highlight surrounding
    replace = "gzr", -- Replace surrounding
    update_n_lines = "gzn", -- Update `n_lines`
  },
})

require('telescope').setup{
  defaults = {
    mappings = {
      i = {
        ['<C-h>'] = 'which_key',
        ['<M-n>'] = require('telescope.actions').insert_original_cword,
      }
    }
  },
  extensions = {
    fzf = {},
    pathogen = {
      attach_mappings = function(map, actions)
        map("i", "<C-o>", actions.proceed_with_parent_dir)
        map("i", "<C-l>", actions.revert_back_last_dir)
        map("i", "<C-b>", actions.change_working_directory)
        map("i", "<C-g>g", actions.grep_in_result)
        map("i", "<C-g>i", actions.invert_grep_in_result)
      end,
      use_last_search_for_live_grep = false,
      -- uses a relative path instead of the full path
      relative_prompt_path = false,
    },
  },
}
require'telescope'.load_extension'fzf'
require'telescope'.load_extension'pathogen'

-- nvim-treesitter (main branch): install missing parsers
do
  local wanted = {'cpp', 'python', 'rust'}
  local installed = require('nvim-treesitter').get_installed('parsers')
  local missing = vim.tbl_filter(function(p) return not vim.list_contains(installed, p) end, wanted)
  if #missing > 0 then require('nvim-treesitter').install(missing) end
end

-- Incremental node selection (replaces nvim-treesitter incremental_selection)
do
  local node
  local function select_node(n)
    if not n then return end
    node = n
    local sr, sc, er, ec = n:range()
    vim.api.nvim_buf_set_mark(0, '<', sr + 1, sc, {})
    vim.api.nvim_buf_set_mark(0, '>', er + 1, ec - 1, {})
    vim.cmd('normal! gv')
  end
  vim.keymap.set({'n','x'}, '<M-o>', function()
    select_node(vim.fn.mode() == 'n' and vim.treesitter.get_node() or node and node:parent())
  end)
  vim.keymap.set('x', '<M-i>', function() select_node(node and node:child(0)) end)
  vim.keymap.set('x', '<M-O>', function()
    if node then
      local p = node:parent()
      while p and p:parent() do p = p:parent() end
      select_node(p)
    end
  end)
end

-- nvim-treesitter-textobjects (main branch)
require('nvim-treesitter-textobjects').setup { select = {lookahead = true}, move = {set_jumps = true} }
do
  local s = require('nvim-treesitter-textobjects.select').select_textobject
  local m = require('nvim-treesitter-textobjects.move')
  local function sel(lhs, obj) vim.keymap.set({'x','o'}, lhs, function() s(obj, 'textobjects') end) end
  local function mov(lhs, fn, obj) vim.keymap.set({'n','x','o'}, lhs, function() m[fn](obj, 'textobjects') end) end
  sel('aa', '@parameter.outer')  sel('ia', '@parameter.inner')
  sel('af', '@function.outer')   sel('if', '@function.inner')
  sel('ac', '@class.outer')      sel('ic', '@class.inner')
  mov(']]', 'goto_next_start',     '@function.outer')
  mov('][', 'goto_next_end',       '@function.outer')
  mov('[[', 'goto_previous_start', '@function.outer')
  mov('[]', 'goto_previous_end',   '@function.outer')
  mov(']c', 'goto_next_start',     '@class.outer')
  mov('[c', 'goto_previous_start', '@class.outer')
  mov(']o', 'goto_next_start',     '@loop.outer')
  mov('[o', 'goto_previous_start', '@loop.outer')
end

require'trouble'.setup()

vim.api.nvim_create_autocmd('LspAttach', {
  callback = function(ev)
    local client = vim.lsp.get_client_by_id(ev.data.client_id)
    local bufnr = ev.buf
    local function map(mode, lhs, rhs, opts)
      local options = {buffer = bufnr}
      if type(opts) == 'string' then
        options.desc = opts
      elseif opts then
        options = vim.tbl_extend('force', options, opts)
      end
      vim.keymap.set(mode, lhs, rhs, options)
    end
    local function nmap(lhs, rhs, opts)
      map('n', lhs, rhs, opts)
    end

    vim.api.nvim_set_option_value('omnifunc', 'v:lua.vim.lsp.omnifunc', {scope='local'})

    nmap('J', '<cmd>Telescope lsp_definitions<cr>', 'Definitions')
    map('i', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>')
    map('v', '=', '<cmd>lua vim.lsp.buf.format()<CR>')
    nmap(',f', '<cmd>lua require("ccls.protocol").request("textDocument/references",{excludeRole=32})<cr>') -- not call
    nmap(',m', '<cmd>lua require("ccls.protocol").request("textDocument/references",{role=64})<cr>') -- macro
    nmap(',r', '<cmd>lua require("ccls.protocol").request("textDocument/references",{role=8})<cr>') -- read
    nmap(',w', '<cmd>lua require("ccls.protocol").request("textDocument/references",{role=16})<cr>') -- write
    nmap('M', '<cmd>Telescope lsp_references<cr>', 'References')
    nmap('<space>=', '<cmd>lua vim.lsp.buf.formatting()<cr>')
    nmap('<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<cr>')
    nmap('<space>lc', '<cmd>lua vim.lsp.buf.code_action()<cr>')
    nmap('<space>li', '<cmd>Inspect<cr>')
    nmap('<space>lr', '<cmd>lua vim.lsp.buf.rename()<cr>', 'Rename')
    nmap('<space>ls', '<cmd>CclsSwitchSourceHeader<cr>', 'Switch source and header')
    nmap('<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<cr>')
    nmap('<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<cr>')
    nmap('<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<cr>')
    nmap('K', '<cmd>lua vim.lsp.buf.hover()<cr>', 'Hover')
    nmap('[e', '<cmd>lua vim.diagnostic.goto_prev()<cr>')
    nmap(']e', '<cmd>lua vim.diagnostic.goto_next()<cr>')
    nmap('gD', '<cmd>lua vim.lsp.buf.declaration()<cr>', 'Declarations')
    nmap('ga', '<cmd>Telescope lsp_dynamic_workspace_symbols<cr>', 'Workspace symbols')
    nmap('x', '<Nop>')
    nmap('xB', '<cmd>CclsBaseHierarchy<cr>')
    nmap('xC', '<cmd>CclsOutgoingCalls<cr>', 'callee')
    nmap('xD', '<cmd>CclsDerivedHierarchy<cr>')
    nmap('xM', '<cmd>CclsMemberHierarchy<cr>', 'member')
    nmap('xb', '<cmd>CclsBase<cr>')
    nmap('xc', '<cmd>CclsIncomingCalls<cr>', 'caller')
    nmap('xd', '<cmd>CclsDerived<cr>')
    nmap('xi', '<cmd>lua vim.lsp.buf.implementation()<cr>', 'Implementation')
    nmap('xm', '<cmd>CclsMember<cr>', 'member')
    nmap('xn', function() M.lsp.words.jump(vim.v.count1) end, 'Next reference')
    nmap('xp', function() M.lsp.words.jump(-vim.v.count1) end, 'Prev reference')
    nmap('xt', '<cmd>lua vim.lsp.buf.type_definition()<cr>', 'Type definition')
    nmap('xv', '<cmd>CclsVars<cr>', 'vars')

    nmap('xh', function() require'my.util'.lsp_get_locations('$ccls/navigate', {direction='U'}) end)
    nmap('xj', function() require'my.util'.lsp_get_locations('$ccls/navigate', {direction='R'}) end)
    nmap('xk', function() require'my.util'.lsp_get_locations('$ccls/navigate', {direction='L'}) end)
    nmap('xl', function() require'my.util'.lsp_get_locations('$ccls/navigate', {direction='D'}) end)

    if client.supports_method 'textDocument/codeLens' then
      vim.api.nvim_create_autocmd({'BufEnter'}, {
        group = vim.api.nvim_create_augroup('lsp_buf_' .. bufnr, {clear = true}),
        buffer = bufnr,
        callback = function(ev)
          vim.lsp.codelens.refresh({bufnr = 0})
        end,
      })
      vim.lsp.codelens.refresh({bufnr = 0})
    end

    if client.supports_method 'textDocument/documentHighlight' then
      vim.api.nvim_create_autocmd({'CursorHold', 'CursorHoldI', 'CursorMoved', 'CursorMovedI'}, {
        group = vim.api.nvim_create_augroup('lsp_word_' .. bufnr, {clear = true}),
        buffer = bufnr,
        callback = function(ev)
          if ev.event:find('CursorMoved') then
            vim.lsp.buf.clear_references()
          else
            vim.lsp.buf.document_highlight()
          end
        end,
        desc = 'Document Highlight',
      })
    end

    if client.server_capabilities.semanticTokensProvider then
      vim.treesitter.stop(bufnr)
    end
  end,
})

vim.lsp.config('ccls', {
  init_options = {
    index = {
      threads = 0,
      initialBlacklist = {"/(test|unittests)/|flang/"},
    },
    highlight = {
      rainbow = 10,
    },
  },
})
vim.lsp.config('lua_ls', {
  on_init = function(client)
    if client.workspace_folders then
      local path = client.workspace_folders[1].name
      if vim.uv.fs_stat(path..'/.luarc.json') or vim.uv.fs_stat(path..'/.luarc.jsonc') then
        return
      end
    end
    client.config.settings.Lua = vim.tbl_deep_extend('force', client.config.settings.Lua, {
      runtime = { version = 'LuaJIT' },
      workspace = {
        checkThirdParty = false,
        library = { vim.env.VIMRUNTIME },
      },
    })
  end,
  settings = { Lua = {} },
})
vim.lsp.config('denols', {
  root_markers = {'deno.json', 'deno.jsonc'},
})
vim.lsp.config('ts_ls', {
  root_markers = {'package.json'},
})
vim.lsp.enable({'ccls', 'lua_ls', 'marksman', 'mlir_lsp_server', 'nimls', 'pyright', 'rust_analyzer', 'tblgen_lsp_server', 'denols', 'ts_ls'})

require('mason').setup()
require('mason-lspconfig').setup()

local func_colors = {
  '#e5b124', '#927754', '#eb992c', '#e2bf8f', '#d67c17',
  '#88651e', '#e4b953', '#a36526', '#b28927', '#d69855',
}
local type_colors = {
  '#e1afc3', '#d533bb', '#9b677f', '#e350b6', '#a04360',
  '#dd82bc', '#de3864', '#ad3f87', '#dd7a90', '#e0438a',
}
local param_colors = {
  '#e5b124', '#927754', '#eb992c', '#e2bf8f', '#d67c17',
  '#88651e', '#e4b953', '#a36526', '#b28927', '#d69855',
}
local var_colors = {
  '#429921', '#58c1a4', '#5ec648', '#36815b', '#83c65d',
  '#419b2f', '#43cc71', '#7eb769', '#58bf89', '#3e9f4a',
}
local all_colors = {
  class = type_colors,
  constructor = func_colors,
  enum = type_colors,
  enumMember = var_colors,
  field = var_colors,
  ['function'] = func_colors,
  method = func_colors,
  parameter = param_colors,
  struct = type_colors,
  typeAlias = type_colors,
  typeParameter = type_colors,
  variable = var_colors
}
for type, colors in pairs(all_colors) do
  for i = 1,#colors do
    for _, lang in pairs({'c', 'cpp'}) do
      vim.api.nvim_set_hl(0, string.format('@lsp.typemod.%s.id%s.%s', type, i-1, lang), {fg=colors[i]})
    end
  end
end

vim.cmd([[
hi @lsp.mod.classScope.cpp gui=italic
hi @lsp.mod.static.cpp gui=bold
hi @lsp.typemod.variable.namespaceScope.cpp gui=bold,underline
]])

require('nvim_comment').setup()

local gitsigns = require('gitsigns')
gitsigns.setup {
  on_attach = function(bufnr)
    local function map(mode, l, r, opts)
      if type(opts) == 'string' then
        opts = { desc = opts }
      else
        opts = {}
      end
      opts.buffer = bufnr
      vim.keymap.set(mode, l, r, opts)
    end
    -- Navigation
    map('n', ']g', function()
      if vim.wo.diff then
        vim.cmd.normal({ ']g', bang = true })
      else
        gitsigns.nav_hunk('next')
      end
    end)

    map('n', '[g', function()
      if vim.wo.diff then
        vim.cmd.normal({ '[g', bang = true })
      else
        gitsigns.nav_hunk('prev')
      end
    end)

    -- Actions
    map('n', '<leader>hs', gitsigns.stage_hunk, 'stage_hunk')
    map('n', '<leader>hr', gitsigns.reset_hunk, 'reset_hunk')
    map('v', '<leader>hs', function() gitsigns.stage_hunk { vim.fn.line('.'), vim.fn.line('v') } end)
    map('v', '<leader>hr', function() gitsigns.reset_hunk { vim.fn.line('.'), vim.fn.line('v') } end)
    map('n', '<leader>hS', gitsigns.stage_buffer, 'stage_buffer')
    map('n', '<leader>hu', gitsigns.undo_stage_hunk, 'undo_stage_hunk')
    map('n', '<leader>hR', gitsigns.reset_buffer, 'reset_buffer')
    map('n', '<leader>hp', gitsigns.preview_hunk, 'preview_hunk')
    map('n', '<leader>hb', function() gitsigns.blame_line { full = true } end)
    map('n', '<leader>hd', gitsigns.diffthis, 'diffthis')
    map('n', '<leader>hD', function() gitsigns.diffthis('~') end, 'diffthis ~')
    -- Text object
    map({'o', 'x'}, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
  end
}

local neogit = require('neogit')
neogit.setup {}

require'neo-tree'.setup {
  window = {
    mappings = {
      ["l"] = "open",
      ["h"] = "close_node",
      ["<space>"] = "none",
      ["Y"] = {
        function(state)
          local node = state.tree:get_node()
          local path = node:get_id()
          vim.fn.setreg("+", path, "c")
        end,
        desc = "Copy Path to Clipboard",
      },
      ["O"] = {
        function(state)
          require("lazy.util").open(state.tree:get_node().path, { system = true })
        end,
        desc = "Open with System Application",
      },
      ["P"] = { "toggle_preview", config = { use_float = false } },
    },
  },
  filesystem = {
    follow_current_file = {
      enabled = true,
      leave_dirs_open = false,
    },
  },
}

-- require('octo').setup()

require('which-key').setup({
  defaults = {},
  spec = {
    {'<leader><tab>', group = 'tabs'},
    {'<leader>a', group = 'app'},
    {'<leader>c', group = 'code'},
    {'<leader>f', group = 'file/find'},
    {'<leader>g', group = 'git'},
    {'<leader>gh', group = 'hunks'},
    {'<leader>gz', group = 'surround'},
    {'<leader>l', group = 'lsp'},
    {'<leader>p', group = 'project'},
    {'<leader>s', group = 'search'},
    {'<leader>t', group = 'toggle/terminal'},
    {'<leader>q', group = 'quit'},
    {'<leader>x', group = 'xref'},
  },
})

require'claudecode'.setup {
  terminal = {
    split_width_percentage = 0.4,
  },
}

return M
