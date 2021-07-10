if not require('lv-utils').check_lsp_client_active 'purescriptls' then
  require 'lsp.purescript'
  require('lspconfig')['purescriptls'].autostart()
end

if packer_plugins['purescript-vim'] and not packer_plugins['purescript-vim'].loaded then
  require('packer').loader 'purescript-vim'
end
