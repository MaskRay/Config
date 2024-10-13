require'hop'.setup()

require('telescope').setup{
  defaults = {
    mappings = {
      i = {
        ['<C-h>'] = 'which_key',
        ['<M-n>'] = require('telescope.actions').insert_original_cword,
      }
    }
  },
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

local treesitter = require 'nvim-treesitter.configs'
treesitter.setup {
  ensure_installed = {'cpp', 'python'},
  highlight = {enable = true},
  textobjects = {
    move = {
      enable = true,
      set_jumps = true,
      goto_next_start = {
        [']m'] = '@class.outer',
        [']]'] = '@function.outer',
      },
      goto_next_end = {
        [']M'] = '@class.outer',
      },
      goto_previous_start = {
        ['[m'] = '@class.outer',
        ['[['] = '@function.outer',
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
    vim.api.nvim_buf_set_keymap(bufnr, mode, lhs, rhs, options)
  end
  local function nmap(lhs, rhs, opts)
    map('n', lhs, rhs, opts)
  end

  local function buf_set_option(name, value) vim.api.nvim_set_option_value(name, value, {scope='local'}) end

  -- Enable completion triggered by <c-x><c-o>
  buf_set_option('omnifunc', 'v:lua.vim.lsp.omnifunc')

  -- See `:help vim.lsp.*` for documentation on any of the below functions
  map('i', '<C-k>', '<cmd>lua vim.lsp.buf.signature_help()<CR>')
  nmap(',f', '<cmd>lua require("ccls.protocol").request("textDocument/references",{excludeRole=32})<cr>') -- not call
  nmap(',m', '<cmd>lua require("ccls.protocol").request("textDocument/references",{role=64})<cr>') -- macro
  nmap(',r', '<cmd>lua require("ccls.protocol").request("textDocument/references",{role=8})<cr>') -- read
  nmap(',w', '<cmd>lua require("ccls.protocol").request("textDocument/references",{role=16})<cr>') -- write
  nmap('<M-,>', '<cmd>lua vim.lsp.buf.references()<CR>', 'References')
  nmap('<M-j>', '<cmd>lua vim.lsp.buf.definition()<CR>', 'Definitions')
  nmap('<space>=', '<cmd>lua vim.lsp.buf.formatting()<CR>')
  nmap('<space>e', '<cmd>lua vim.lsp.diagnostic.show_line_diagnostics()<CR>')
  nmap('<space>lc', '<cmd>lua vim.lsp.buf.code_action()<CR>')
  nmap('<space>lr', '<cmd>lua vim.lsp.buf.rename()<CR>', 'Rename')
  nmap('<space>wa', '<cmd>lua vim.lsp.buf.add_workspace_folder()<CR>')
  nmap('<space>wl', '<cmd>lua print(vim.inspect(vim.lsp.buf.list_workspace_folders()))<CR>')
  nmap('<space>wr', '<cmd>lua vim.lsp.buf.remove_workspace_folder()<CR>')
  nmap('K', '<cmd>lua vim.lsp.buf.hover()<CR>', 'Hover')
  nmap('[e', '<cmd>lua vim.diagnostic.goto_prev()<CR>')
  nmap(']e', '<cmd>lua vim.diagnostic.goto_next()<CR>')
  nmap('gD', '<cmd>lua vim.lsp.buf.declaration()<CR>', 'Declarations')
  nmap('xB', '<cmd>CclsBaseHierarchy<cr>')
  nmap('xC', '<cmd>CclsOutgoingCalls<cr>', 'callee')
  nmap('xD', '<cmd>CclsDerivedHierarchy<cr>')
  nmap('xM', '<cmd>CclsMemberHierarchy<cr>', 'member')
  nmap('xb', '<cmd>CclsBase<cr>')
  nmap('xc', '<cmd>CclsIncomingCalls<cr>', 'caller')
  nmap('xd', '<cmd>CclsDerived<cr>')
  nmap('xi', '<cmd>lua vim.lsp.buf.implementation()<CR>', 'Implementation')
  nmap('xm', '<cmd>CclsMember<cr>', 'member')
  nmap('xt', '<cmd>lua vim.lsp.buf.type_definition()<CR>', 'Type definition')
  nmap('xv', '<cmd>CclsVars<cr>', 'vars')
end
local servers = {'ccls', 'lua_ls', 'nimls', 'pyright', 'rust_analyzer', 'ts_ls'}
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
  snippet = {expand = function(args) vim.fn["UltiSnips#Anon"](args.body) end},
  sources = {
    {name = 'buffer'}, {name = 'nvim_lsp'}, {name = "ultisnips"},
    {name = "nvim_lua"}, {name = "look"}, {name = "path"},
    {name = 'cmp_tabnine'}, {name = "calc"}, {name = "spell"},
    {name = "emoji"}
  },
  completion = {completeopt = 'menu,menuone,noinsert'}
}

vim.api.nvim_exec('hi default link CocHoverRange NONE', true)

require('mason').setup()
require('mason-lspconfig').setup()

require('nvim_comment').setup()

-- Default map: <leader>h ]c [c
require('gitsigns').setup()

local neogit = require('neogit')
neogit.setup {}

require('octo').setup()

require('which-key').setup()
