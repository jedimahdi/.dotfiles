local ok, harpoon = pcall(require, "harpoon")
if not ok then
  return
end

local u = require("config.utils")

harpoon.setup({})

u.nmap("<leader>sa", require("harpoon.mark").add_file)
u.nmap("<leader>sm", require("harpoon.ui").toggle_quick_menu)

for i = 1, 5 do
  u.nmap(string.format("<leader>%s", i), function()
    require("harpoon.ui").nav_file(i)
  end)
end
