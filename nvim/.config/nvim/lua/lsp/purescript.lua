require("lspconfig").purescriptls.setup({
	-- cmd = { DATA_PATH .. '/lspinstall/purescript/node_modules/.bin/purescript-language-server', '--stdio' },
	handlers = {
		["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
			underline = false,
			virtual_text = false,
		}),
	},
	settings = { purescript = { formatter = "purs-tidy" } },
})
