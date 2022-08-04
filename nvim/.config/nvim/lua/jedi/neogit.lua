local neogit = require("neogit")
local nnoremap = require("jedi.keymap").nnoremap

neogit.setup({})

nnoremap("<leader>gs", function()
  neogit.open({})
end)

nnoremap("<leader>ga", "<cmd>!git fetch --all<CR>")
