if not require('lv-utils').check_lsp_client_active 'html' then
  require 'lsp.json'
  require('lspconfig')['html'].autostart()
  require 'lsp.efm'
  require('lspconfig')['efm'].autostart()
end
