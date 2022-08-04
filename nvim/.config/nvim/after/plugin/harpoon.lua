local ok, harpoon = pcall(require, "harpoon")
if not ok then
  return
end

local Remap = require("jedi.keymap")
local nnoremap = Remap.nnoremap

harpoon.setup({})

nnoremap("<leader>sa", require("harpoon.mark").add_file)
nnoremap("<leader>sm", require("harpoon.ui").toggle_quick_menu)

for i = 1, 5 do
  nnoremap(string.format("<leader>%s", i), function()
    require("harpoon.ui").nav_file(i)
  end)
end
