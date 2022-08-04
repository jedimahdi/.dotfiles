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
