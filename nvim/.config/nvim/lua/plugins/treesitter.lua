local M = {}

M.setup = function()
  local status_ok, treesitter_configs = pcall(require, "nvim-treesitter.configs")
  if not status_ok then
    return
  end

  treesitter_configs.setup({
    ensure_installed = {
      "bash",
      "javascript",
      "json",
      "lua",
      "typescript",
      "tsx",
      "html",
      "css",
      "c",
      "cpp",
      "toml",
      "scss",
      "yaml",
      "nix",
      "latex",
      "elm",
      "dockerfile",
      "clojure",
      "cmake",
    },
    -- ensure_installed = 'maintained',
    highlight = { enable = true, disable = { "haskell" } },
    indent = { enable = true },
    autotag = { enable = true },
    autopairs = { enable = true },
    context_commentstring = {
      enable = true,
    },
  })
end

return M
