local u = require("config.utils")

local lsp = vim.lsp

-- border = double, rounded, single, shadow, none
local border_opts = { border = "rounded", focusable = true, scope = "line" }

-- vim.diagnostic.config({ virtual_text = false, float = border_opts })

lsp.handlers["textDocument/signatureHelp"] = lsp.with(lsp.handlers.signature_help, border_opts)
lsp.handlers["textDocument/hover"] = lsp.with(lsp.handlers.hover, border_opts)

-- use lsp formatting if it's available (and if it's good)
-- otherwise, fall back to null-ls
-- local preferred_formatting_clients = { "hls", "elmls" }
-- local fallback_formatting_client = "null-ls"

local augroup = vim.api.nvim_create_augroup("LspFormatting", {})

local lsp_formatting = function(bufnr)
  local clients = vim.lsp.get_active_clients({ bufnr = bufnr })
  lsp.buf.format({
    bufnr = bufnr,
    async = true,
    filter = function(client)
      if client.name == "tsserver" then
        return false
      end
      return true
    end,
  })
end

vim.g.autoformating = true

local function toggleAutoFormatting(bufnr)
  if vim.g.autoformating == true then
    vim.g.autoformating = false
    vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
  else
    vim.g.autoformating = true
    vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
    vim.api.nvim_create_autocmd("BufWritePre", {
      group = augroup,
      buffer = bufnr,
      command = "LspFormatting",
    })
  end
end

local on_attach = function(client, bufnr)
  -- commands
  u.buf_command(bufnr, "LspHover", vim.lsp.buf.hover)
  u.buf_command(bufnr, "LspDiagPrev", vim.diagnostic.goto_prev)
  u.buf_command(bufnr, "LspDiagNext", vim.diagnostic.goto_next)
  u.buf_command(bufnr, "LspDiagLine", vim.diagnostic.open_float)
  u.buf_command(bufnr, "LspDiagQuickfix", vim.diagnostic.setqflist)
  u.buf_command(bufnr, "LspSignatureHelp", vim.lsp.buf.signature_help)
  u.buf_command(bufnr, "LspTypeDef", vim.lsp.buf.type_definition)
  u.buf_command(bufnr, "LspRangeAct", vim.lsp.buf.range_code_action)
  u.buf_command(bufnr, "LspRename", function()
    vim.lsp.buf.rename()
  end)

  -- bindings
  u.buf_map(bufnr, "n", "gi", ":LspRename<CR>")
  u.buf_map(bufnr, "n", "K", ":LspHover<CR>")
  u.buf_map(bufnr, "n", "[a", ":LspDiagPrev<CR>")
  u.buf_map(bufnr, "n", "]a", ":LspDiagNext<CR>")
  u.buf_map(bufnr, "n", "<Leader>a", ":LspDiagLine<CR>")
  u.buf_map(bufnr, "i", "<C-x><C-x>", "<cmd> LspSignatureHelp<CR>")

  u.buf_map(bufnr, "n", "gy", ":LspTypeDef<CR>")
  u.buf_map(bufnr, "n", "gr", ":LspRef<CR>")
  u.buf_map(bufnr, "n", "gd", ":LspDef<CR>")
  u.buf_map(bufnr, "n", "ga", ":LspAct<CR>")
  u.buf_map(bufnr, "v", "ga", "<Esc><cmd> LspRangeAct<CR>")

  if client.supports_method("textDocument/formatting") then
    u.buf_command(bufnr, "LspFormatting", function()
      lsp_formatting(bufnr)
    end)

    u.buf_map(bufnr, "n", "<Leader>p", ":LspFormatting<CR>")

    u.buf_map(bufnr, "n", "<Leader>P", function()
      toggleAutoFormatting(bufnr)
    end)

    vim.api.nvim_clear_autocmds({ group = augroup, buffer = bufnr })
    vim.api.nvim_create_autocmd("BufWritePre", {
      group = augroup,
      buffer = bufnr,
      command = "LspFormatting",
    })
  end
end

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities = require("cmp_nvim_lsp").update_capabilities(capabilities)

for _, server in ipairs({
  "sumneko",
  "tsserver",
  "hls",
  "purescriptls",
  "null-ls",
  "elm",
  "pyright",
  "ccls",
  "rust_analyzer",
  "rnix",
}) do
  require("lsp." .. server).setup(on_attach, capabilities)
end

-- suppress irrelevant messages
local notify = vim.notify
vim.notify = function(msg, ...)
  if msg:match("%[lspconfig%]") then
    return
  end

  if msg:match("warning: multiple different client offset_encodings") then
    return
  end

  notify(msg, ...)
end
