local M = {}
M.setup = function(on_attach, capabilities)
  local server_opts = {
    on_attach = on_attach,
    capabilities = capabilities,
  }
  -- require("lspconfig").idris2_lsp.setup({ on_attach, capabilities })
  local idris2_status_ok, idris2 = pcall(require, "idris2")

  if idris2_status_ok then
    idris2.setup({ server = server_opts })
  end
end

return M
