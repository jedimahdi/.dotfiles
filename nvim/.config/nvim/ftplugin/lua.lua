if not require("lv-utils").check_lsp_client_active("sumneko_lua") then
	require("lsp.lua")
	require("lspconfig")["sumneko_lua"].autostart()

	local null_ls_ok, null_ls = pcall(require, "null-ls")
	if not null_ls_ok then
		return
	end

	local sources = { null_ls.builtins.formatting.stylua }

	null_ls.config({ sources = sources })
	require("lspconfig")["null-ls"].setup({})
	require('lspconfig')['null-ls'].autostart()
end

vim.cmd("setl ts=2 sw=2")
