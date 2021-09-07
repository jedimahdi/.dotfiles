local telescope_ok, telescope = pcall(require, "telescope")
local actions_ok, actions = pcall(require, "telescope.actions")
if not telescope_ok or not actions_ok then
  return
end

telescope.setup({
  defaults = {
    initial_mode = "insert",
    sorting_strategy = "ascending",
    file_previewer = require("telescope.previewers").vim_buffer_cat.new,
    grep_previewer = require("telescope.previewers").vim_buffer_vimgrep.new,
    qflist_previewer = require("telescope.previewers").vim_buffer_qflist.new,
    file_sorter = require("telescope.sorters").get_fzy_sorter,
    generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,

    layout_config = { prompt_position = "top" },

    mappings = {
      i = {
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
        ["<Esc>"] = actions.close,
      },
      n = {
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
        ["<Esc>"] = actions.close,
      },
    },
  },
})
