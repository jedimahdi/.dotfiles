if not require('lv-utils').check_lsp_client_active 'sumneko_lua' then
  require 'lsp.lua'
  require 'lsp.efm'
  require('lspconfig')['sumneko_lua'].autostart()
  require('lspconfig')['efm'].autostart()
end

vim.cmd 'setl ts=2 sw=2'
