local lspconfig = require("lspconfig")

local M = {}
M.setup = function(on_attach, capabilities)
  capabilities.textDocument.completion.completionItem.snippetSupport = false

  local server_opts = {
    on_attach = on_attach,
    capabilities = capabilities,
  }
  local rust_tools_status_ok, rust_tools = pcall(require, "rust-tools")

  if rust_tools_status_ok then
    rust_tools.setup({ server = server_opts })
  else
    lspconfig.rust_analyzer.setup(server_opts)
  end
end

return M
