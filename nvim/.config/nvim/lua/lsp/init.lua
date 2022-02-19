local u = require("config.utils")

local lsp = vim.lsp
local api = vim.api

local border_opts = { border = "single", focusable = false, scope = "line" }

vim.diagnostic.config({ virtual_text = false, float = border_opts })

lsp.handlers["textDocument/signatureHelp"] = lsp.with(lsp.handlers.signature_help, border_opts)
lsp.handlers["textDocument/hover"] = lsp.with(lsp.handlers.hover, border_opts)

-- use lsp formatting if it's available (and if it's good)
-- otherwise, fall back to null-ls
local preferred_formatting_clients = { "hls", "elmls" }
local fallback_formatting_client = "null-ls"

local formatting = function(bufnr)
  bufnr = tonumber(bufnr) or api.nvim_get_current_buf()

  local selected_client
  for _, client in ipairs(lsp.buf_get_clients(bufnr)) do
    if vim.tbl_contains(preferred_formatting_clients, client.name) then
      selected_client = client
      break
    end

    if client.name == fallback_formatting_client then
      selected_client = client
    end
  end

  if not selected_client then
    return
  end

  local params = lsp.util.make_formatting_params()
  selected_client.request("textDocument/formatting", params, function(err, res)
    if err then
      local err_msg = type(err) == "string" and err or err.message
      vim.notify("global.lsp.formatting: " .. err_msg, vim.log.levels.WARN)
      return
    end

    if not api.nvim_buf_is_loaded(bufnr) or api.nvim_buf_get_option(bufnr, "modified") then
      return
    end

    if res then
      lsp.util.apply_text_edits(res, bufnr, selected_client.offset_encoding or "utf-16")
      api.nvim_buf_call(bufnr, function()
        vim.cmd("silent noautocmd update")
      end)
    end
  end, bufnr)
end

global.lsp = {
  border_opts = border_opts,
  formatting = formatting,
}

local on_attach = function(client, bufnr)
  -- commands
  u.command("LspFormatting", vim.lsp.buf.formatting)
  u.command("LspHover", vim.lsp.buf.hover)
  u.command("LspRename", vim.lsp.buf.rename)
  u.command("LspDiagPrev", vim.diagnostic.goto_prev)
  u.command("LspDiagNext", vim.diagnostic.goto_next)
  u.command("LspDiagLine", vim.diagnostic.open_float)
  u.command("LspDiagQuickfix", vim.diagnostic.setqflist)
  u.command("LspSignatureHelp", vim.lsp.buf.signature_help)
  u.command("LspTypeDef", vim.lsp.buf.type_definition)
  u.command("LspRangeAct", vim.lsp.buf.range_code_action)

  -- bindings
  u.buf_map(bufnr, "n", "gi", ":LspRename<CR>")
  u.buf_map(bufnr, "n", "gy", ":LspTypeDef<CR>")
  u.buf_map(bufnr, "n", "K", ":LspHover<CR>")
  u.buf_map(bufnr, "n", "[a", ":LspDiagPrev<CR>")
  u.buf_map(bufnr, "n", "]a", ":LspDiagNext<CR>")
  u.buf_map(bufnr, "n", "<Leader>a", ":LspDiagLine<CR>")
  u.buf_map(bufnr, "n", "<Leader>q", ":LspDiagQuickfix<CR>")
  u.buf_map(bufnr, "n", "<Leader>p", ":LspFormatting<CR>")
  u.buf_map(bufnr, "i", "<C-x><C-x>", "<cmd> LspSignatureHelp<CR>")

  u.buf_map(bufnr, "n", "gr", ":LspRef<CR>")
  u.buf_map(bufnr, "n", "gd", ":LspDef<CR>")
  u.buf_map(bufnr, "n", "ga", ":LspAct<CR>")
  u.buf_map(bufnr, "v", "ga", "<Esc><cmd> LspRangeAct<CR>")

  if client.supports_method("textDocument/formatting") then
    vim.cmd([[
        augroup LspFormatting
            autocmd! * <buffer>
            autocmd BufWritePost <buffer> silent! lua global.lsp.formatting(vim.fn.expand("<abuf>"))
        augroup END
        ]])
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
  "sumneko",
  "elm",
  "pyright",
  "ccls",
}) do
  require("lsp." .. server).setup(on_attach, capabilities)
end

-- suppress lspconfig messages
local notify = vim.notify
vim.notify = function(msg, ...)
  if msg:match("%[lspconfig%]") then
    return
  end

  notify(msg, ...)
end
