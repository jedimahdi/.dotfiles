require'lspconfig'.purescriptls.setup {
  cmd = { DATA_PATH .. '/lspinstall/purescript/node_modules/.bin/purescript-language-server', '--stdio' },
}
