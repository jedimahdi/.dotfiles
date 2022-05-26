local M = {}

M.setup = function()
  local status_ok, treesitter_configs = pcall(require, "nvim-treesitter.configs")
  if not status_ok then
    return
  end

  treesitter_configs.setup({
    ensure_installed = {
      "bash",
      "c",
      "cmake",
      "cpp",
      "css",
      "dart",
      "dockerfile",
      "elixir",
      "elm",
      "fish",
      "go",
      "haskell",
      "hjson",
      "html",
      "java",
      "javascript",
      "jsdoc",
      "json",
      "lua",
      "make",
      "latex",
      "markdown",
      "nix",
      "ocaml",
      "perl",
      "php",
      "prisma",
      "python",
      "rasi",
      "ruby",
      "rust",
      "scala",
      "scheme",
      "scss",
      "solidity",
      "toml",
      "tsx",
      "typescript",
      "vim",
      "vue",
    },
    highlight = { enable = true },
    indent = { enable = true },
    autotag = { enable = true },
    autopairs = { enable = false },
    context_commentstring = {
      enable = true,
    },
  })
end

return M
