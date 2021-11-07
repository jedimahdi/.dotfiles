local lspconfig = require("lspconfig")

local M = {}
M.setup = function(on_attach, capabilities)
  lspconfig.jedi_language_server.setup({
    on_attach = on_attach,
    capabilities = capabilities,
  })
end

return M
