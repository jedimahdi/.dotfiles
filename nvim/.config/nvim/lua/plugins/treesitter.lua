local M = {}

M.setup = function()
  local status_ok, treesitter_configs = pcall(require, "nvim-treesitter.configs")
  if not status_ok then
    return
  end

  treesitter_configs.setup({
    ensure_installed = "maintained",
    -- ensure_installed = 'maintained',
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
