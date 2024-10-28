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

return M
