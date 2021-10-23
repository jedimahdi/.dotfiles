require("nvim-autopairs").setup({
  disable_filetype = { "TelescopePrompt", "vim" },
})

require("nvim-autopairs.completion.cmp").setup({
  map_cr = true, --  map <CR> on insert mode
  map_complete = true, -- it will auto insert `(` (map_char) after select function or method item
  auto_select = false, -- automatically select the first item
  map_char = { -- modifies the function or method delimiter by filetypes
    all = "(",
    tex = "{",
  },
})

-- require("nvim-autopairs.completion.cmp").setup({
--   map_complete = false,
--   insert = true,
-- })
