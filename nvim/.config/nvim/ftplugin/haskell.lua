local ht = require("haskell-tools")
local buffer = vim.api.nvim_get_current_buf()
local def_opts = { noremap = true, silent = true }
ht.start_or_attach({
  hls = {
    on_attach = function(client, bufnr)
      local opts = vim.tbl_extend("keep", def_opts, { buffer = bufnr })

      local nmap = function(keys, func, desc)
        if desc then
          desc = "LSP: " .. desc
        end

        vim.keymap.set("n", keys, func, { buffer = bufnr, desc = desc })
      end

      local imap = function(keys, func, desc)
        if desc then
          desc = "LSP: " .. desc
        end

        vim.keymap.set("i", keys, func, { buffer = bufnr, desc = desc })
      end

      nmap("K", vim.lsp.buf.hover, "Hover Documentation")
      nmap("<leader>gr", vim.lsp.buf.rename, "Rename Symbol")
      nmap("ga", vim.lsp.buf.code_action, "Code Action")
      nmap("gd", vim.lsp.buf.definition, "Goto Definition")
      nmap("<leader>di", vim.lsp.buf.implementation, "Goto Implementation")
      nmap("gr", require("telescope.builtin").lsp_references, "References")
      nmap("<leader>ds", require("telescope.builtin").lsp_document_symbols, "Document Symbols")
      nmap("<leader>dw", require("telescope.builtin").lsp_dynamic_workspace_symbols, "Workspace Symbols")
      imap("<C-x>", vim.lsp.buf.signature_help, "Signature Documentation")
      nmap("<leader>cr", vim.lsp.codelens.refresh, "Refresh Code Lens")
      nmap("<leader>cd", vim.lsp.codelens.display, "Display Code Lens")
      -- haskell-language-server relies heavily on codeLenses,
      -- so auto-refresh (see advanced configuration) is enabled by default
      vim.keymap.set("n", "<space>ca", vim.lsp.codelens.run, opts)
      vim.keymap.set("n", "<space>hs", ht.hoogle.hoogle_signature, opts)
      -- vim.keymap.set("n", "<space>ea", ht.lsp.buf_eval_all, opts)
    end,
  },
})

-- Suggested keymaps that do not depend on haskell-language-server:
local bufnr = vim.api.nvim_get_current_buf()
-- set buffer = bufnr in ftplugin/haskell.lua
local opts = { noremap = true, silent = true, buffer = bufnr }

-- Toggle a GHCi repl for the current package
vim.keymap.set("n", "<leader>rr", ht.repl.toggle, opts)
-- Toggle a GHCi repl for the current buffer
vim.keymap.set("n", "<leader>rf", function()
  ht.repl.toggle(vim.api.nvim_buf_get_name(0))
end, def_opts)
vim.keymap.set("n", "<leader>rq", ht.repl.quit, opts)
