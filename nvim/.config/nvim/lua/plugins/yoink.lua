local u = require("utils")

vim.g.yoinkMoveCursorToEndOfPaste = true
vim.g.yoinkIncludeDeleteOperations = true

local opts = { noremap = false }

u.nmap("[y", "<Plug>(YoinkPostPasteSwapBack)", opts)
u.nmap("]y", "<Plug>(YoinkPostPasteSwapForward)", opts)

u.nmap("[y", "<Plug>(YoinkPostPasteSwapBack)", opts)
u.nmap("]y", "<Plug>(YoinkPostPasteSwapForward)", opts)

u.nmap("y", "<Plug>(YoinkYankPreserveCursorPosition)", opts)
u.xmap("y", "<Plug>(YoinkYankPreserveCursorPosition)", opts)

u.nmap("p", "<Plug>(YoinkPaste_p)", opts)
