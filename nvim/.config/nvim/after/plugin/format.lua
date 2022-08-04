local Remap = require("jedi.keymap")
local nnoremap = Remap.nnoremap

nnoremap("<leader>p", function()
  local ft = vim.bo.ft

  if ft == "haskell" then
    vim.lsp.buf.format()
  else
    vim.cmd("Neoformat")
  end
end)

vim.g.neoformat_enabled_haskell = { "stylishhaskell" }
