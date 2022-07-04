local u = require("config.utils")

local api = vim.api

-- const myString = "hello ${}" ->
-- const myString = `hello ${}`
local change_template_string_quotes = function()
  local row, col = unpack(api.nvim_win_get_cursor(0))
  row = row - 1

  local quote_start, quote_end
  u.gfind(api.nvim_get_current_line(), "[\"']", function(pos)
    if not quote_start then
      -- start at first quote
      quote_start = pos
    elseif pos < col then
      -- move start if quote is closer to col
      if (pos - col) > (quote_start - col) then
        quote_start = pos
      end
    elseif not quote_end then
      -- first quote after col is end
      quote_end = pos
    end
  end)

  -- if found, replace quotes with backticks
  if quote_start and quote_start <= col and quote_end then
    api.nvim_buf_set_text(0, row, quote_start - 1, row, quote_start, { "`" })
    api.nvim_buf_set_text(0, row, quote_end - 1, row, quote_end, { "`" })
  end

  -- input and move cursor into pair
  u.input("${}", "n")
  u.input("<Left>")
end

local M = {}
M.setup = function(on_attach, capabilities)
  require("typescript").setup({
    server = {
      on_attach = function(client, bufnr)
        u.buf_map(bufnr, "n", "gs", ":TypescriptRemoveUnused<CR>")
        u.buf_map(bufnr, "n", "gS", ":TypescriptOrganizeImports<CR>")
        u.buf_map(bufnr, "n", "go", ":TypescriptAddMissingImports<CR>")
        u.buf_map(bufnr, "n", "gA", ":TypescriptFixAll<CR>")
        u.buf_map(bufnr, "n", "gI", ":TypescriptRenameFile<CR>")
        u.buf_map(bufnr, "i", "${", change_template_string_quotes, { nowait = true })

        on_attach(client, bufnr)
      end,
      capabilities = capabilities,
    },
  })
end

return M
