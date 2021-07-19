if not require('lv-utils').check_lsp_client_active 'jsonls' then
  require 'lsp.json'
  require('lspconfig')['jsonls'].autostart()
end
