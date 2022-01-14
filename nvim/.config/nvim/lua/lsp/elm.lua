-- npm install -g elm elm-test elm-format @elm-tooling/elm-language-server
local lspconfig = require("lspconfig")

local M = {}
M.setup = function(on_attach, capabilities)
  lspconfig.elmls.setup({
    on_attach = on_attach,
    capabilities = capabilities,
  })
end

return M
