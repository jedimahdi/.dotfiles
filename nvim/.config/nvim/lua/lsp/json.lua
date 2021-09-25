-- npm install -g vscode-json-languageserver

-- local schemas = nil
-- local status_ok, jsonls_settings = pcall(require, "nlspsettings.jsonls")
-- if status_ok then
--   schemas = jsonls_settings.get_default_schemas()
-- end

require("lspconfig").jsonls.setup({
  -- cmd = { "json-languageserver", "--stdio" },
  cmd = {
    "node",
    DATA_PATH .. "/lspinstall/json/vscode-json/json-language-features/server/dist/node/jsonServerMain.js",
    "--stdio",
  },
  -- settings = {
  --   json = {
  --     schemas = schemas,
  --   },
  -- },
  commands = {
    Format = {
      function()
        vim.lsp.buf.range_formatting({}, { 0, 0 }, { vim.fn.line("$"), 0 })
      end,
    },
  },
})
