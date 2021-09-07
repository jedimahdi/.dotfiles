if not require('lv-utils').check_lsp_client_active 'html' then
  require 'lsp.html'
  require('lspconfig')['html'].autostart()

	local null_ls_ok, null_ls = pcall(require, "null-ls")
	if not null_ls_ok then
		return
	end

	local sources = { null_ls.builtins.formatting.prettier }

	null_ls.config({ sources = sources })
	require("lspconfig")["null-ls"].setup({})
	require('lspconfig')['null-ls'].autostart()
end
