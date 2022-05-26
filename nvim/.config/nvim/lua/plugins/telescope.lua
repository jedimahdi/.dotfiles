local telescope = require("telescope")
local actions = require("telescope.actions")

local u = require("config.utils")
local commands = require("config.commands")

telescope.setup({
  picker = {
    hidden = false,
  },
  extensions = {
    fzf = {
      fuzzy = true,
      override_generic_sorter = true,
      override_file_sorter = true,
      case_mode = "smart_case",
    },
  },

  defaults = {
    vimgrep_arguments = {
      "rg",
      "--color=never",
      "--no-heading",
      "--with-filename",
      "--line-number",
      "--column",
      "--no-ignore",
      "--smart-case",
      "--hidden",
    },
    prompt_prefix = "  🔍   ",
    selection_caret = "  ",
    entry_prefix = "  ",
    initial_mode = "insert",
    selection_strategy = "reset",
    sorting_strategy = "ascending",
    layout_strategy = "horizontal",
    layout_config = {
      horizontal = {
        prompt_position = "top",
        preview_width = 0.55,
        results_width = 0.8,
      },
      vertical = {
        mirror = false,
      },
      width = 0.80,
      height = 0.85,
      preview_cutoff = 120,
    },
    border = {},
    borderchars = { "" },
    color_devicons = true,
    set_env = { ["COLORTERM"] = "truecolor" },
    path_display = { "absolute" },

    file_sorter = require("telescope.sorters").get_fuzzy_file,
    file_ignore_patterns = {
      "dist*",
      "node_modules",
      ".git/",
      "dist/",
      [[elm.stuff]],
    },
    generic_sorter = require("telescope.sorters").get_generic_fuzzy_sorter,

    mappings = {
      i = {
        ["<C-j>"] = actions.move_selection_next,
        ["<C-k>"] = actions.move_selection_previous,
        ["<Esc>"] = actions.close,
        ["<C-u>"] = false,
        ["<M-u>"] = actions.preview_scrolling_up,
        ["<M-d>"] = actions.preview_scrolling_down,
        ["<C-h>"] = "which_key",
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

local find_files = function()
  local set = require("telescope.actions.set")
  local builtin = require("telescope.builtin")

  local opts = {
    attach_mappings = function(_, map)
      map("i", "<C-v>", function(prompt_bufnr)
        set.edit(prompt_bufnr, "vsplit")
      end)

      -- edit file and matching test file in split
      map("i", "<C-f>", function(prompt_bufnr)
        set.edit(prompt_bufnr, "edit")
        commands.edit_test_file("vsplit $FILE | wincmd w")
      end)

      return true
    end,
  }

  local is_git_project = pcall(builtin.git_files, opts)
  if not is_git_project then
    builtin.find_files(opts)
  end
end

u.command("Files", find_files)

u.command("Rg", "Telescope live_grep")
u.command("BLines", "Telescope current_buffer_fuzzy_find")
u.command("History", "Telescope oldfiles")
u.command("Buffers", "Telescope buffers")
u.command("BCommits", "Telescope git_bcommits")
u.command("Commits", "Telescope git_commits")
u.command("HelpTags", "Telescope help_tags")
u.command("ManPages", "Telescope man_pages")

u.map("n", "<leader>f", "<cmd>Files<CR>")
u.map("n", "<leader>F", "<cmd>Telescope find_files<CR>")
u.map("n", "<leader>sw", "<cmd>Rg<CR>")
u.map("n", "<leader>sb", "<cmd>Buffers<CR>")
u.map("n", "<leader>so", "<cmd>History<CR>")
u.map("n", "<leader>sh", "<cmd>HelpTags<CR>")
u.map("n", "<leader>sl", "<cmd>BLines<CR>")
u.nmap("<leader>sc", "<cmd>BCommits<CR>")
u.map("n", "<leader>tc", "<cmd>Telescope colorscheme<CR>")

-- lsp
u.command("LspRef", "Telescope lsp_references")
u.command("LspDef", "Telescope lsp_definitions")
u.command("LspSym", "Telescope lsp_workspace_symbols")
u.command("LspAct", function()
  vim.lsp.buf.code_action()
end)
