local telescope_ok, telescope = pcall(require, "telescope")
local actions_ok, actions = pcall(require, "telescope.actions")
if not telescope_ok or not actions_ok then
  return
end

telescope.setup({
  extensions = {
    fzf = { fuzzy = true, override_generic_sorter = true, override_file_sorter = true },
  },
  defaults = {
    initial_mode = "insert",
    sorting_strategy = "ascending",

    layout_config = { prompt_position = "top" },
    vimgrep_arguments = {
      "rg",
      "--color=never",
      "--no-heading",
      "--with-filename",
      "--line-number",
      "--column",
      "--smart-case",
      "--ignore",
      "--hidden",
      "-g",
      "!.git",
    },

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

telescope.load_extension("fzf")
