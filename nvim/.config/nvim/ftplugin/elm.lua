if not require("lv-utils").check_lsp_client_active("elmls") then
  require("lsp.elm")
  require("lspconfig")["elmls"].autostart()
end
