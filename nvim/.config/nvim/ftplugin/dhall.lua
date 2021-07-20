if not require('lv-utils').check_lsp_client_active 'dhall_lsp_server' then
  require 'lsp.dhall'
  require('lspconfig')['dhall_lsp_server'].autostart()
end

if packer_plugins['dhall-vim'] and not packer_plugins['dhall-vim'].loaded then
  require('packer').loader 'dhall-vim'
end
