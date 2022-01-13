-- npm install -g elm elm-test elm-format @elm-tooling/elm-language-server
local lspconfig = require("lspconfig")

local M = {}
M.setup = function(on_attach, capabilities)
  lspconfig.elmls.setup({
    -- cmd = { DATA_PATH .. "/lspinstall/elm/node_modules/.bin/elm-language-server" },
    -- init_options = {
    --   elmAnalyseTrigger = "change",
    --   elmFormatPath = DATA_PATH .. "/lspinstall/elm/node_modules/.bin/elm-format",
    --   elmPath = DATA_PATH .. "/lspinstall/elm/node_modules/.bin/elm",
    --   elmTestPath = DATA_PATH .. "/lspinstall/elm/node_modules/.bin/elm-test",
    -- },
    on_attach = on_attach,
    capabilities = capabilities,
  })
end

return M
