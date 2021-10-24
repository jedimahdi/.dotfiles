local lspconfig = require("lspconfig")

local M = {}
M.setup = function(on_attach, capabilities)
  lspconfig.ccls.setup({
    init_options = {
      clang = {
        extraArgs = { "-std=c++20" },
      },
    },
    on_attach = on_attach,
    capabilities = capabilities,
  })
end

return M
