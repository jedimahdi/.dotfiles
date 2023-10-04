local actions = require("telescope.actions")

require("telescope").setup({
  extensions = {
    fzf = {
      fuzzy = true, -- false will only do exact matching
      override_generic_sorter = true, -- override the generic sorter
      override_file_sorter = true, -- override the file sorter
      case_mode = "smart_case", -- or "ignore_case" or "respect_case"
      -- the default case_mode is "smart_case"
    },
  },
  -- pickers = {
  --   find_files = {
  --     find_command = { "rg", "--files", "--hidden", "--glob", "!**/.git/*" },
  --   },
  -- },
  defaults = {
    -- file_previewer = require("telescope.previewers").vim_buffer_cat.new,
    -- file_sorter = require("telescope.sorters").get_fuzzy_file,
    file_ignore_patterns = {
      "dist*",
      "node_modules",
      "output",
      ".git/",
      "dist/",
      [[elm.stuff]],
    },

    mappings = {
      i = {
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
        ["<Esc>"] = actions.close,
        ["<C-x>"] = false,
        ["<C-q>"] = actions.send_to_qflist,
      },
      n = {
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
        ["<Esc>"] = actions.close,
      },
    },
  },
})

require("telescope").load_extension("fzf")
-- require("telescope").load_extension("git_worktree")

vim.keymap.set("n", "<leader>/", function()
  -- You can pass additional configuration to telescope to change theme, layout, etc.
  require("telescope.builtin").current_buffer_fuzzy_find(require("telescope.themes").get_dropdown({
    winblend = 10,
    previewer = false,
  }))
end, { desc = "[/] Fuzzily search in current buffer]" })

vim.keymap.set("n", "<leader>f", require("telescope.builtin").find_files, { desc = "[S]earch [F]iles" })
vim.keymap.set("n", "<leader>F", require("telescope.builtin").find_files, { desc = "Search Git Files" })
vim.keymap.set("n", "<leader>sh", require("telescope.builtin").help_tags, { desc = "[S]earch [H]elp" })
vim.keymap.set("n", "<leader>sw", require("telescope.builtin").grep_string, { desc = "[S]earch current [W]ord" })
vim.keymap.set("n", "<leader>sg", require("telescope.builtin").live_grep, { desc = "[S]earch by [G]rep" })
vim.keymap.set("n", "<leader>sb", require("telescope.builtin").buffers, { desc = "[ ] Find existing buffers" })
vim.keymap.set("n", "<leader>sd", require("telescope.builtin").diagnostics, { desc = "[S]earch [D]iagnostics" })
vim.keymap.set("n", "<leader>x", require("telescope.builtin").commands, { desc = "Commands" })

local Remap = require("jedi.keymap")
local nnoremap = Remap.nnoremap
nnoremap("<leader>gc", function()
  require("jedi.telescope").git_branches()
end)
nnoremap("<leader>gw", function()
  require("telescope").extensions.git_worktree.git_worktrees()
end)
nnoremap("<leader>gm", function()
  require("telescope").extensions.git_worktree.create_git_worktree()
end)
nnoremap("<leader>td", function()
  require("jedi.telescope").dev()
end)
