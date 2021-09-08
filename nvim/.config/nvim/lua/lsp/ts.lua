require("lspconfig").tsserver.setup({
  -- cmd = { DATA_PATH .. '/lspinstall/typescript/node_modules/.bin/typescript-language-server', '--stdio' },
  filetypes = { "javascript", "javascriptreact", "javascript.jsx", "typescript", "typescriptreact", "typescript.tsx" },
  on_attach = function(client)
    client.resolved_capabilities.document_formatting = false
  end,
  settings = { documentFormatting = false },
  handlers = {
    ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
      -- virtual_text = { spacing = 0, prefix = "" },
      -- virtual_text = false,
      signs = true,
      underline = false,
      update_in_insert = false,
    }),
  },
})
