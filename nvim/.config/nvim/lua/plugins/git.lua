local u = require("utils")

require("gitsigns").setup()

u.nmap("<leader>gb", "<cmd>Telescope git_branches<CR>")
u.nmap("<leader>go", "<cmd>Telescope git_status<CR>")
