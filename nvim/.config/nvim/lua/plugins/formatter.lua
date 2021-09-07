local function prettier()
  return {
    exe = "prettier",
    args = { "--stdin-filepath", vim.api.nvim_buf_get_name(0), "--single-quote" },
    stdin = true,
  }
end

local function stylua()
  return {
    exe = "stylua",
    args = {
      "--search-parent-directories",
      "--stdin-filepath",
      vim.api.nvim_buf_get_name(0),
    },
    stdin = false,
  }
end

local function stylishHaskell()
  return {
    exe = "stylish-haskell",
    args = {
      "-i",
    },
    stdin = false,
  }
end

require("formatter").setup({
  logging = false,
  filetype = {
    javascript = { prettier },
    typescript = { prettier },
    lua = { stylua },
    haskell = { stylishHaskell },
  },
})
