local lspconfig = require("lspconfig")

local M = {}
M.setup = function(on_attach, capabilities)
  lspconfig.hls.setup({
    on_attach = on_attach,
    settings = { languageServerHaskell = { formattingProvider = "stylish-haskell" } },
    capabilities = capabilities,
  })
end

return M
