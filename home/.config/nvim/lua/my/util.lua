local M = {}

function M.get_root()
  local file = vim.api.nvim_buf_get_name(0)
  if vim.startswith(file, vim.env.HOME .. '/.config') then
    return vim.env.HOME .. '/Config/home'
  end
  local clients = vim.lsp.get_clients()
  for _, client in pairs(clients) do
    if client.root_dir then
      return client.root_dir
    end
  end
  return vim.fs.root(0, {'.git', '.hg', 'nvim'})
end

function M.blame_line(opts)
  opts = vim.tbl_deep_extend("force", {
    count = 3,
    filetype = "git",
    size = {
      width = 0.8,
      height = 0.8,
    },
    border = "rounded",
  }, opts or {})
  local cursor = vim.api.nvim_win_get_cursor(0)
  local line = cursor[1]
  local file = vim.api.nvim_buf_get_name(0)
  local cmd = { "git", "-C", M.get_root(), "log", "-n", opts.count, "-u", "-L", line .. ",+1:" .. file }
  return require("lazy.util").float_cmd(cmd, opts)
end

function M.lazygit(opts)
  local cmd = {'lazygit'}
  vim.list_extend(cmd, opts.args or {})
  require'lazy.util'.float_term(cmd, opts)
end

local util = require 'lspconfig.util'
function M.switch_source_header(bufnr)
  bufnr = util.validate_bufnr(bufnr)
  local client = util.get_active_client_by_name(bufnr, 'ccls')
  local params = { uri = vim.uri_from_bufnr(bufnr) }
  if client then
    client.request('textDocument/switchSourceHeader', params, function(err, result)
      if err then
        error(tostring(err))
      end
      if not result then
        print 'Corresponding file cannot be determined'
        return
      end
      vim.api.nvim_command('edit ' .. vim.uri_to_fname(result))
    end, bufnr)
  else
    print 'method textDocument/switchSourceHeader is not supported by any servers active on the current buffer'
  end
end

-- based on runtime/lua/vim/lsp/buf.lua:get_locations, added `extra`
local lsp = vim.lsp
local api = vim.api
function M.lsp_get_locations(method, extra)
  local util = require 'vim.lsp.util'
  local opts = {}
  local bufnr = api.nvim_get_current_buf()
  local clients = lsp.get_clients({ method = method, bufnr = bufnr })
  if not next(clients) then
    vim.notify(lsp._unsupported_method(method), vim.log.levels.WARN)
    return
  end
  local win = api.nvim_get_current_win()
  local from = vim.fn.getpos('.')
  from[1] = bufnr
  local tagname = vim.fn.expand('<cword>')
  local remaining = #clients

  ---@type vim.quickfix.entry[]
  local all_items = {}

  ---@param result nil|lsp.Location|lsp.Location[]
  ---@param client vim.lsp.Client
  local function on_response(_, result, client)
    local locations = {}
    if result then
      locations = vim.islist(result) and result or { result }
    end
    local items = util.locations_to_items(locations, client.offset_encoding)
    vim.list_extend(all_items, items)
    remaining = remaining - 1
    if remaining == 0 then
      if vim.tbl_isempty(all_items) then
        vim.notify('No locations found', vim.log.levels.INFO)
        return
      end

      local title = 'LSP locations'
      if opts.on_list then
        assert(vim.is_callable(opts.on_list), 'on_list is not a function')
        opts.on_list({
          title = title,
          items = all_items,
          context = { bufnr = bufnr, method = method },
        })
        return
      end

      if #all_items == 1 then
        local item = all_items[1]
        local b = item.bufnr or vim.fn.bufadd(item.filename)

        -- Save position in jumplist
        vim.cmd("normal! m'")
        -- Push a new item into tagstack
        local tagstack = { { tagname = tagname, from = from } }
        vim.fn.settagstack(vim.fn.win_getid(win), { items = tagstack }, 't')

        vim.bo[b].buflisted = true
        local w = opts.reuse_win and vim.fn.win_findbuf(b)[1] or win
        api.nvim_win_set_buf(w, b)
        api.nvim_win_set_cursor(w, { item.lnum, item.col - 1 })
        vim._with({ win = w }, function()
          -- Open folds under the cursor
          vim.cmd('normal! zv')
        end)
        return
      end
      if opts.loclist then
        vim.fn.setloclist(0, {}, ' ', { title = title, items = all_items })
        vim.cmd.lopen()
      else
        vim.fn.setqflist({}, ' ', { title = title, items = all_items })
        vim.cmd('botright copen')
      end
    end
  end
  for _, client in ipairs(clients) do
    local params = util.make_position_params(win, client.offset_encoding)
    params = vim.tbl_extend('force', params, extra or {})
    client:request(method, params, function(_, result)
      on_response(_, result, client)
    end)
  end
end

return M
