local u = require("utils")

require("gitsigns").setup()

-- u.nmap("<leader>gb", "<cmd>Telescope git_branches<CR>")
-- u.nmap("<leader>go", "<cmd>Telescope git_status<CR>")
--
-- u.nmap("<Leader>G", ":tab Git<CR>")
-- u.nmap("<Leader>g", ":Git ", { silent = false })

vim.cmd("autocmd FileType fugitive nmap <buffer> <Tab> =")
