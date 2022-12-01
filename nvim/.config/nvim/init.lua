require("jedi.set")
require("jedi.packer")
require("jedi.mappings")
require("jedi.telescope")
require("jedi.lir")
require("jedi.colorscheme")
require("jedi.treesitter")
require("jedi.lsp")
require("jedi.cmp")
require("jedi.harpoon")

-- Turn on status information
require("fidget").setup()

require("Comment").setup()
require("nvim-autopairs").setup({})

-- Snippet
require("snippy").setup({
  mappings = {
    is = {
      ["<C-j>"] = "expand_or_advance",
      ["<C-k>"] = "previous",
    },
  },
  scopes = {
    typescriptreact = { "typescript" },
  },
})
