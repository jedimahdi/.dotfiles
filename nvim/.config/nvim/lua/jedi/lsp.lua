local lspconfig = require("lspconfig")

vim.keymap.set("n", "[d", vim.diagnostic.goto_prev)
vim.keymap.set("n", "]d", vim.diagnostic.goto_next)
vim.keymap.set("n", "<leader>a", vim.diagnostic.open_float)

local on_attach = function(_, bufnr)
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
  nmap("<leader>ca", vim.lsp.codelens.run, "Run Code Lens")
  nmap("<leader>cd", vim.lsp.codelens.display, "Display Code Lens")
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

local function merge(t1, t2)
  for k, v in ipairs(t2) do
    table.insert(t1, v)
  end

  return t1
end

require("mason").setup()

local servers = { "clangd", "pyright" }
local just_ensure_installed_servers = { "tsserver" }
local ensure_installed_servers = merge(servers, just_ensure_installed_servers)

require("mason-lspconfig").setup({
  ensure_installed = ensure_installed_servers,
})

for _, lsp in ipairs(servers) do
  lspconfig[lsp].setup({
    on_attach = on_attach,
    capabilities = capabilities,
  })
end

require("typescript-tools").setup({
  on_attach = on_attach,
  capabilities = capabilities,
  settings = {
    tsserver_file_preferences = {
      disableSuggestions = true,
    },
  },
})

-- lspconfig.purescriptls.setup({
--   on_attach = on_attach,
--   capabilities = capabilities,
-- })

-- lspconfig.emmet_ls.setup({})

-- lspconfig.tsserver.setup({
--   on_attach = function(x, bufnr)
--     on_attach(x, bufnr)
--     -- require("lsp_signature").on_attach({}, bufnr)
--   end,
--   capabilities = capabilities,
--   handlers = {
--     ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
--       underline = false,
--     }),
--   },
-- })

-- local capabilitiesWithSnippet = capabilities
-- capabilitiesWithSnippet.textDocument.completion.completionItem.snippetSupport = true

-- lspconfig.jsonls.setup({
--   on_attach = on_attach,
--   capabilities = capabilities,
--   settings = {
--     json = {
--       schemas = require("schemastore").json.schemas({
--         select = {
--           ".eslintrc",
--           "package.json",
--           "tsconfig.json",
--           "prettierrc.json",
--           "tslint.json",
--         },
--       }),
--       validate = { enable = true },
--     },
--   },
-- })

-- lspconfig.elmls.setup({
--   on_attach = on_attach,
--   capabilities = capabilities,
-- })

-- lspconfig.hls.setup({
--   on_attach = on_attach,
--   capabilities = capabilities,
--   handlers = {
--     ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
--       underline = false,
--       virtual_text = false,
--     }),
--   },
--   settings = {
--     haskell = {
--       formattingProvider = "stylish-haskell",
--     },
--   },
-- })

local rt = require("rust-tools")

rt.setup({
  server = {
    on_attach = function(x, bufnr)
      on_attach(x, bufnr)
      -- Hover actions
      -- vim.keymap.set("n", "<C-space>", rt.hover_actions.hover_actions, { buffer = bufnr })
      -- Code action groups
      vim.keymap.set("n", "gA", rt.code_action_group.code_action_group, { buffer = bufnr })
    end,
    handlers = {
      ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
        underline = false,
        virtual_text = false,
      }),
    },
  },
})

-- lspconfig.rust_analyzer.setup({
--   on_attach = on_attach,
--   capabilities = require("cmp_nvim_lsp").default_capabilities(
--     vim.lsp.protocol.make_client_capabilities(),
--     { snippetSupport = false }
--   ),
--   cmd = { "rustup", "run", "nightly", "rust-analyzer" },
--   handlers = {
--     ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
--       underline = false,
--     }),
--   },
-- })

-- Make runtime files discoverable to the server
-- local runtime_path = vim.split(package.path, ";")
-- table.insert(runtime_path, "lua/?.lua")
-- table.insert(runtime_path, "lua/?/init.lua")
-- require("lspconfig").lua_ls.setup({
--   on_attach = on_attach,
--   capabilities = capabilities,
--   settings = {
--     Lua = {
--       runtime = {
--         -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
--         version = "LuaJIT",
--         path = runtime_path,
--       },
--       diagnostics = {
--         -- Get the language server to recognize the `vim` global
--         globals = { "vim" },
--       },
--       workspace = {
--         -- Make the server aware of Neovim runtime files
--         library = vim.api.nvim_get_runtime_file("", true),
--         checkThirdParty = false,
--       },
--       -- Do not send telemetry data containing a randomized but unique identifier
--       telemetry = {
--         enable = false,
--       },
--     },
--   },
-- })

-- require("lspconfig").ocamllsp.setup({
--   on_attach = on_attach,
--   capabilities = capabilities,
-- })

local def_opts = { noremap = true, silent = true }

vim.g.haskell_tools = {
  hls = {
    on_attach = function(_, bufnr, ht)
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
    settings = {
      haskell = {
        formattingProvider = "fourmolu",
      },
    },
  },
}
