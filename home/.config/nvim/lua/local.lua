require('overseer').load_template('user.cpp_build')

-- local autocmd = vim.api.nvim_create_autocmd
-- autocmd("FileType", {
--     pattern = "mlir",
--     callback = function()
--         local root_dir = vim.fs.dirname(
--             vim.fs.find({ '.git' }, { upward = true })[1]
--         )
--         local client = vim.lsp.start({
--             name = 'mlir',
--             cmd = { 'mlir-lsp-server' },
--             root_dir = root_dir,
--         })
--         vim.lsp.buf_attach_client(0, client)
--     end
-- })
