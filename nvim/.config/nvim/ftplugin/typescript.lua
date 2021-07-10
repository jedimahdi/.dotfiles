if not require('lv-utils').check_lsp_client_active 'tsserver' then
  require 'lsp.ts'
  require 'lsp.efm'
  require('lspconfig')['tsserver'].autostart()
  require('lspconfig')['efm'].autostart()
end
