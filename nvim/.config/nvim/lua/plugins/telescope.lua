local telescope = require("telescope")
local actions = require("telescope.actions")

local u = require("utils")
local commands = require("commands")

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
        ["<C-u>"] = false,
        ["<M-u>"] = actions.preview_scrolling_up,
        ["<M-d>"] = actions.preview_scrolling_down,
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

global.telescope = {
  -- try git_files and fall back to find_files
  find_files = function()
    local set = require("telescope.actions.set")
    local builtin = require("telescope.builtin")

    local current = vim.api.nvim_get_current_buf()
    local opts = {
      attach_mappings = function(_, map)
        map("i", "<C-v>", function(prompt_bufnr)
          set.edit(prompt_bufnr, "Vsplit")
        end)

        -- replace current buffer
        map("i", "<C-r>", function(prompt_bufnr)
          set.edit(prompt_bufnr, "edit")
          commands.bdelete(current)
        end)

        -- close all other buffers
        map("i", "<C-x>", function(prompt_bufnr)
          set.edit(prompt_bufnr, "edit")
          commands.bonly()
        end)

        -- edit file and matching test file in split
        map("i", "<C-f>", function(prompt_bufnr)
          set.edit(prompt_bufnr, "edit")
          commands.edit_test_file("Vsplit", function()
            vim.cmd("wincmd w")
          end)
        end)

        return true
      end,
    }

    local is_git_project = pcall(builtin.git_files, opts)
    if not is_git_project then
      builtin.find_files(opts)
    end
  end,
}

u.lua_command("Files", "global.telescope.find_files()")
u.command("Rg", "Telescope live_grep")
u.command("BLines", "Telescope current_buffer_fuzzy_find")
u.command("History", "Telescope oldfiles")
u.command("Buffers", "Telescope buffers")
u.command("BCommits", "Telescope git_bcommits")
u.command("Commits", "Telescope git_commits")
u.command("HelpTags", "Telescope help_tags")
u.command("ManPages", "Telescope man_pages")

u.map("n", "<Leader>f", "<cmd>Files<CR>")
u.map("n", "<Leader>F", "<cmd>Telescope find_files<CR>")
u.map("n", "<Leader>w", "<cmd>Rg<CR>")
u.map("n", "<Leader>b", "<cmd>Buffers<CR>")
u.map("n", "<Leader>so", "<cmd>History<CR>")
u.map("n", "<Leader>sh", "<cmd>HelpTags<CR>")
u.map("n", "<Leader>sl", "<cmd>BLines<CR>")
u.map("n", "<Leader>ss", "<cmd>LspSym<CR>")
u.map("n", "<Leader>tc", "<cmd>Telescope colorscheme<CR>")

-- lsp
u.command("LspRef", "Telescope lsp_references")
u.command("LspDef", "Telescope lsp_definitions")
u.command("LspSym", "Telescope lsp_workspace_symbols")
u.command("LspAct", "Telescope lsp_code_actions")
u.command("LspRangeAct", "Telescope lsp_range_code_actions")
