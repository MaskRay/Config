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

local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.c3 = {
  install_info = {
    url = "https://github.com/c3lang/tree-sitter-c3",
    files = {"src/parser.c", "src/scanner.c"},
    branch = "main",
  },
}

require'leap'.add_default_mappings(true)
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
  extensions = {fzf = {}},
  pickers = {
    find_files = {
      mappings = {
        i = {
          ['<C-w>'] = function(prompt_bufnr)
            -- find parent directory https://github.com/nvim-telescope/telescope.nvim/issues/2179
            local current_picker = require('telescope.actions.state').get_current_picker(prompt_bufnr)
            local cwd = current_picker.cwd and tostring(current_picker.cwd)
            or vim.loop.cwd()
            local parent_dir = vim.fs.dirname(cwd)

            require('telescope.actions').close(prompt_bufnr)
            require('telescope.builtin').find_files {
              prompt_title = vim.fs.basename(parent_dir),
              cwd = parent_dir,
            }
          end,
        },
      },
    },
  },
}
require'telescope'.load_extension'fzf'

local treesitter = require 'nvim-treesitter.configs'
treesitter.setup {
  ensure_installed = {'cpp', 'python'},
  highlight = { enable = true },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "<M-o>",
      scope_incremental = "<M-O>",
      node_incremental = "<M-o>",
      node_decremental = "<M-i>",
    },
  },
  textobjects = {
    lsp_interop = {
      enable = true,
      border = 'none',
      floating_preview_opts = {},
      peek_definition_code = {
        ["<leader>df"] = "@function.outer",
        ["<leader>dF"] = "@class.outer",
      },
    },
    move = {
      enable = true,
      set_jumps = true,
      goto_next_start = {
        [']m'] = '@class.outer',
        [']]'] = '@function.outer',
        [']o'] = '@loop.*',
      },
      goto_next_end = {
        [']M'] = '@class.outer',
      },
      goto_previous_start = {
        ['[m'] = '@class.outer',
        ['[['] = '@function.outer',
        ['[o'] = '@loop.*',
      },
      goto_previous_end = {
        ['[M'] = '@class.outer',
      },
    },
    select = {
      enable = true,
      lookahead = true,
      keymaps = {
        ['aa'] = '@parameter.outer',
        ['ia'] = '@parameter.inner',
        ['af'] = '@function.outer',
        ['if'] = '@function.inner',
        ['ac'] = '@class.outer',
        ['ic'] = '@class.inner',
      },
    },
  },
}

require'trouble'.setup()

local nvim_lsp = require 'lspconfig'
-- Use an on_attach function to only map the following keys
-- after the language server attaches to the current buffer
local on_attach = function(client, bufnr)
  local function map(mode, lhs, rhs, opts)
    local options = {noremap = true, silent = true}
    if opts then
      if type(opts) == 'string' then
        opts = {desc = opts}
      end
      options = vim.tbl_extend('force', options, opts)
    end
    if type(opts) == 'string' then
      vim.api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, options)
    else
      vim.keymap.set(mode, lhs, rhs, {buffer=true})
    end
  end
  local function nmap(lhs, rhs, opts)
    map('n', lhs, rhs, opts)
  end

  local function buf_set_option(name, value) vim.api.nvim_set_option_value(name, value, {scope='local'}) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- See `:help vim.lsp.*` for documentation on any of the below functions
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
end

local servers = {'ccls', 'lua_ls', 'marksman', 'mlir_lsp_server', 'nimls', 'pyright', 'rust_analyzer', 'tblgen_lsp_server'}
for _, lsp in ipairs(servers) do
  local options = {
    on_attach = on_attach,
    flags = {
      debounce_text_changes = 150,
    }
  }
  if lsp == 'ccls' then
    options = vim.tbl_extend('force', options, {
      init_options = {
        index = {
          threads = 0,
          initialBlacklist = {"/(test|unittests)/"},
        };
        highlight = {
          rainbow = 10,
        };
      }
    })
  elseif lsp == 'lua_ls' then
    options = vim.tbl_extend('force', options, {
      on_init = function(client)
        if client.workspace_folders then
          local path = client.workspace_folders[1].name
          if vim.uv.fs_stat(path..'/.luarc.json') or vim.uv.fs_stat(path..'/.luarc.jsonc') then
            return
          end
        end

        client.config.settings.Lua = vim.tbl_deep_extend('force', client.config.settings.Lua, {
          runtime = {
            -- Tell the language server which version of Lua you're using
            -- (most likely LuaJIT in the case of Neovim)
            version = 'LuaJIT'
          },
          -- Make the server aware of Neovim runtime files
          workspace = {
            checkThirdParty = false,
            library = {
              vim.env.VIMRUNTIME
              -- Depending on the usage, you might want to add additional paths here.
              -- "${3rd}/luv/library"
              -- "${3rd}/busted/library",
            }
            -- or pull in all of 'runtimepath'. NOTE: this is a lot slower
            -- library = vim.api.nvim_get_runtime_file("", true)
          }
        })
      end,
      settings = {
        Lua = {}
      }
    })
  end
  nvim_lsp[lsp].setup(options)
end
nvim_lsp.denols.setup {
  on_attach = on_attach,
  root_dir = nvim_lsp.util.root_pattern('deno.json', 'deno.jsonc'),
}
nvim_lsp.ts_ls.setup {
  on_attach = on_attach,
  root_dir = nvim_lsp.util.root_pattern('package.json'),
  single_file_support = false,
}

local cmp = require('cmp')
cmp.setup {
  formatting = {
    format = function(entry, vim_item)
      vim_item.menu = ({
        buffer = "[Buffer]",
        nvim_lsp = "[LSP]",
        ultisnips = "[UltiSnips]",
        nvim_lua = "[Lua]",
        cmp_tabnine = "[TabNine]",
        look = "[Look]",
        path = "[Path]",
        spell = "[Spell]",
        calc = "[Calc]",
        emoji = "[Emoji]"
      })[entry.source.name]
      return vim_item
    end
  },
  mapping = {
    ['<C-p>'] = cmp.mapping.select_prev_item(),
    ['<C-n>'] = cmp.mapping.select_next_item(),
    ['<C-d>'] = cmp.mapping.scroll_docs(-4),
    ['<C-f>'] = cmp.mapping.scroll_docs(4),
    ['<C-Space>'] = cmp.mapping.complete(),
    ['<C-e>'] = cmp.mapping.close(),
    ['<Tab>'] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Insert,
      select = true
    }),
  },
  sources = {
    {name = 'buffer'}, {name = 'nvim_lsp'}, {name = "ultisnips"},
    {name = "nvim_lua"}, {name = "look"}, {name = "path"},
    {name = 'cmp_tabnine'}, {name = "calc"}, {name = "spell"},
    {name = "emoji"}
  },
  completion = {completeopt = 'menu,menuone,noinsert'}
}

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
    map('n', ']c', function()
      if vim.wo.diff then
        vim.cmd.normal({ ']c', bang = true })
      else
        gitsigns.nav_hunk('next')
      end
    end)

    map('n', '[c', function()
      if vim.wo.diff then
        vim.cmd.normal({ '[c', bang = true })
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

require('which-key').setup()

local dap = require('dap')
local dapui = require('dapui')
dapui.setup()
require'dap-python'.setup('python3')
require'mason-nvim-dap'.setup()

dap.listeners.before.attach.dapui_config = function()
  dapui.open()
end
dap.listeners.before.launch.dapui_config = function()
  dapui.open()
end
dap.listeners.before.event_terminated.dapui_config = function()
  dapui.close()
end
dap.listeners.before.event_exited.dapui_config = function()
  dapui.close()
end

dap.adapters.gdb = {
  id = 'gdb',
  type = 'executable',
  command = 'gdb',
  args = { '--interpreter=dap' },
}
dap.configurations.c = {
  {
    name = "Launch",
    type = "gdb",
    request = "launch",
    program = function()
      return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
    end,
    args = function()
      local args_str = vim.fn.input({
        prompt = 'Arguments: ',
      })
      return vim.split(args_str, ' +')
    end,
    cwd = "${workspaceFolder}",
    stopAtBeginningOfMainSubprogram = true,
  },
  {
    name = 'Attach to process (GDB)',
    type = 'gdb',
    request = 'attach',
    processId = require('dap.utils').pick_process,
  },
  {
    name = "rr connect",
    type = "gdb",
    request = "launch",
    cwd = "${workspaceFolder}",
    program = function()
      return vim.fn.input('Path to executable: ', vim.fn.getcwd() .. '/', 'file')
    end,
    stopAtBeginningOfMainSubprogram = true,
    MIMode = "gdb",
    miDebuggerServerAddress = "localhost:5050",
    serverLaunchTimeout = 5000,
    postRemoteConnectCommands = {
      {
        text = "monitor reset",
        ignoreFailures = false
      },
      {
        text = "load",
        ignoreFailures = false
      },
    },
  }
}
dap.configurations.cpp = dap.configurations.c
dap.configurations.rust = {}

dap.reverse_continue = function()
  local s = dap.session()
  if not s then return end
  s:evaluate("-exec set exec-direction reverse")
  dap.continue()
  s:evaluate("-exec set exec-direction forward")
end
dap.reverse_step_over = function()
  local s = require("dap").session()
  if not s then return end
  s:evaluate("-exec set exec-direction reverse")
  dap.step_over()
  s:evaluate("-exec set exec-direction forward")
end
dap.reverse_step_out = function()
  local s = require("dap").session()
  if not s then return end
  s:evaluate("-exec set exec-direction reverse")
  dap.step_out()
  s:evaluate("-exec set exec-direction forward")
end
dap.reverse_step_into = function()
  local s = require("dap").session()
  if not s then return end
  s:evaluate("-exec set exec-direction reverse")
  dap.step_into()
  s:evaluate("-exec set exec-direction forward")
end

vim.keymap.set('n', '<F1>', dap.terminate)
vim.keymap.set('n', '<F5>', dap.toggle_breakpoint)
vim.keymap.set('n', '<F8>', dap.continue)
vim.keymap.set('n', '<F10>', dap.step_over)
vim.keymap.set('n', '<F11>', dap.step_into)
vim.keymap.set('n', '<F12>', dap.step_out)
vim.keymap.set('n', '<A-up>', dap.down)
vim.keymap.set('n', '<A-down>', dap.up)
vim.keymap.set('n', 'g<F8>', dap.continue)
vim.keymap.set('n', 'g<F10>', dap.reverse_step_over)
vim.keymap.set('n', 'g<F11>', dap.reverse_step_into)
vim.keymap.set('n', 'g<F12>', dap.reverse_step_out)

return M
