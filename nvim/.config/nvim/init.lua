require("jedi.set")
require("jedi.lazy")
require("jedi.mappings")
require("jedi.telescope")
require("jedi.lir")
require("jedi.colorscheme")
require("jedi.treesitter")
require("jedi.lsp")
require("jedi.cmp")
require("jedi.harpoon")
require("jedi.tmux")
require("jedi.dap")
require("jedi.formatter")

require("Comment").setup()
require("nvim-autopairs").setup({})

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

local yank_group = augroup("HighlightYank", {})
autocmd("TextYankPost", {
  group = yank_group,
  pattern = "*",
  callback = function()
    vim.highlight.on_yank({
      higroup = "IncSearch",
      timeout = 100,
    })
  end,
})
