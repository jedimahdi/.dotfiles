require('lspconfig').hls.setup {
  filetypes = { 'haskell', 'lhaskell' },
  settings = { languageServerHaskell = { formattingProvider = 'stylish-haskell' } },
  handlers = {
    ['textDocument/publishDiagnostics'] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
      underline = false,
      virtual_text = false,
    }),
  },
  -- cmd = {DATA_PATH .. "/lspinstall/haskell/hls"},
}
