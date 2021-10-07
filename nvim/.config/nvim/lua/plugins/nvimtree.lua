local nvim_tree_config = require("nvim-tree.config")
local tree_cb = nvim_tree_config.nvim_tree_callback

local u = require("utils")

vim.g.nvim_tree_ignore = { ".git", "node_modules", ".cache", "dist-newstyle", "elm-stuff" }

require("nvim-tree").setup({
  disable_netrw = false,
  hijack_netrw = false,
  update_focused_file = {
    enable = true,
  },
  view = {
    mappings = {
      list = {
        { key = { "<CR>", "l" }, cb = tree_cb("edit") },
        { key = "h", cb = tree_cb("close_node") },
        { key = "o", cb = tree_cb("edit") .. tree_cb("close") },
        { key = "v", cb = tree_cb("vsplit") },
      },
    },
  },
})

u.nmap("<leader>e", "<cmd>NvimTreeToggle<CR>")
