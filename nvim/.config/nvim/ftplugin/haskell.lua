if not require("lv-utils").check_lsp_client_active("hls") then
  require("lsp.haskell")
  require("lspconfig")["hls"].autostart()
end

if packer_plugins["haskell-vim"] and not packer_plugins["haskell-vim"].loaded then
  require("packer").loader("haskell-vim")
end
