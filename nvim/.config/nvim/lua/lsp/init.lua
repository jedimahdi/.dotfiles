-- C++
-- require'lspconfig'.clangd.setup {}

-- add LspLog and LspRestart
function _G.reload_lsp()
  vim.lsp.stop_client(vim.lsp.get_active_clients())
  vim.cmd [[edit]]
end

function _G.open_lsp_log()
  local path = vim.lsp.get_log_path()
  vim.cmd('edit ' .. path)
end

vim.cmd([[ command! Format execute 'lua vim.lsp.buf.formatting()' ]])
vim.cmd('command! -nargs=0 LspLog call v:lua.open_lsp_log()')
-- vim.cmd('command! -nargs=0 LspRestart call v:lua.reload_lsp()')
