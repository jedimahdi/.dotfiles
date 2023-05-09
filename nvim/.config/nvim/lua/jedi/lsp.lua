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

local capabilities = require("cmp_nvim_lsp").default_capabilities()
-- local capabilities = vim.lsp.protocol.make_client_capabilities()
-- capabilities = require("cmp_nvim_lsp").default_capabilities(capabilities)

lspconfig.tsserver.setup({
  on_attach = function(x, bufnr)
    on_attach(x, bufnr)
    require("lsp_signature").on_attach({}, bufnr)
  end,
  capabilities = capabilities,
  handlers = {
    ["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
      underline = false,
    }),
  },
})

-- local capabilitiesWithSnippet = capabilities
-- capabilitiesWithSnippet.textDocument.completion.completionItem.snippetSupport = true

lspconfig.jsonls.setup({
  on_attach = on_attach,
  capabilities = capabilities,
  settings = {
    json = {
      schemas = require('schemastore').json.schemas {
        select = {
          '.eslintrc',
          'package.json',
          'tsconfig.json',
          'prettierrc.json',
          'tslint.json',
        },
      },
      validate = { enable = true },
    },
  },
})

-- lspconfig.elmls.setup({
--   on_attach = on_attach,
--   capabilities = capabilities,
-- })

lspconfig.purescriptls.setup({
  on_attach = on_attach,
  capabilities = capabilities,
})

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

require("neodev").setup({})

require("lspconfig").lua_ls.setup({
  on_attach = on_attach,
  capabilities = capabilities,
  settings = {
    Lua = {
      runtime = {
        -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
        version = "LuaJIT",
      },
      diagnostics = {
        -- Get the language server to recognize the `vim` global
        globals = { "vim" },
      },
      workspace = {
        -- Make the server aware of Neovim runtime files
        library = vim.api.nvim_get_runtime_file("", true),
      },
      -- Do not send telemetry data containing a randomized but unique identifier
      telemetry = {
        enable = false,
      },
    },
  },
})

-- lspconfig.sumneko_lua.setup({
--   on_attach = on_attach,
--   capabilities = capabilities,
--   cmd = { sumneko_binary, "-E", sumneko_root_path .. "/main.lua" },
--   settings = {
--     Lua = {
--       runtime = {
--         -- Tell the language server which version of Lua you're using (most likely LuaJIT in the case of Neovim)
--         version = "LuaJIT",
--         -- Setup your lua path
--         path = vim.split(package.path, ";", {}),
--       },
--       diagnostics = {
--         -- Get the language server to recognize the `vim` global
--         globals = { "vim" },
--       },
--       workspace = {
--         -- Make the server aware of Neovim runtime files
--         library = {
--           [vim.fn.expand("$VIMRUNTIME/lua")] = true,
--           [vim.fn.expand("$VIMRUNTIME/lua/vim/lsp")] = true,
--         },
--       },
--     },
--   },
-- })

-- vim.api.nvim_create_autocmd({ "BufEnter", "BufWinEnter" }, {
--   pattern = { "*.txt" },
--   callback = function()
--     -- local util = require("lspconfig.util")
--
--     vim.lsp.start_client({
--       name = "lsp_demo",
--       cmd = {
--         "/home/mahdi/apps/haskell/lsp-demo/dist-newstyle/build/x86_64-linux/ghc-9.0.2/lsp-demo-1.1.2/x/lsp-demo/build/lsp-demo/lsp-demo",
--       },
--       -- root_dir = util.root_pattern(".git"),
--     })
--   end,
-- })
