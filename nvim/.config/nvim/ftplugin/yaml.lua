if not require("lv-utils").check_lsp_client_active("yamlls") then
  require("lsp.yaml")
  require("lspconfig")["yamlls"].autostart()
end
