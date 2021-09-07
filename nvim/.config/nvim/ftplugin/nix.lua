-- if packer_plugins['vim-nix'] and not packer_plugins['vim-nix'].loaded then require('packer').loader 'vim-nix' end
if not require("lv-utils").check_lsp_client_active("null-ls") then
	local null_ls_ok, null_ls = pcall(require, "null-ls")
	if not null_ls_ok then
		return
	end

	local sources = { null_ls.builtins.formatting.nixfmt }

	null_ls.config({ sources = sources })
	require("lspconfig")["null-ls"].setup({})
	-- require('lspconfig')['null-ls'].autostart()
end
