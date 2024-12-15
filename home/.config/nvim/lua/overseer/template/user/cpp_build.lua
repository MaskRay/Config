-- /home/stevearc/.config/nvim/lua/overseer/template/user/cpp_build.lua
return {
  name = "C++ build",
  builder = function()
    local file = vim.fn.expand("%:p")
    local re = vim.regex('/ccls')
    if re:match_str(file) then
      return {
        cmd = { "ninja" },
        args = { '-C', '/home/ray/ccls/out/debug' },
        components = { { "on_output_quickfix", open = true }, "default" },
      }
    end
    return {
      cmd = { "ninja" },
      args = { '-C', '/tmp/Debug', 'lld' },
      components = { { "on_output_quickfix", open = true }, "default" },
    }
  end,
  condition = {
    filetype = { "cpp" },
  },
}
