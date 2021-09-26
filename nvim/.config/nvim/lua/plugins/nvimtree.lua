local status_ok, nvim_tree_config = pcall(require, "nvim-tree.config")
if not status_ok then
  return
end

local tree_cb = nvim_tree_config.nvim_tree_callback

vim.o.termguicolors = true
vim.g.nvim_tree_ignore = { ".git", "node_modules", ".cache" }
vim.g.nvim_tree_follow = 1
vim.g.nvim_tree_update_cwd = 1
vim.g.nvim_tree_respect_buf_cwd = 1

vim.g.nvim_tree_bindings = {
  { key = { "<CR>", "l" }, cb = tree_cb("edit") },
  { key = "h", cb = tree_cb("close_node") },
  { key = "o", cb = tree_cb("edit") .. tree_cb("close") },
  { key = "v", cb = tree_cb("vsplit") },
}
