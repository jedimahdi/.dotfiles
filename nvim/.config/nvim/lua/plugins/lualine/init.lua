local components = require("plugins.lualine.components")
local lualine = require("lualine")
local onedarker = require("plugins.lualine.themes.onedarker")

lualine.setup({
  options = {
    theme = onedarker,
    icons_enabled = true,
    component_separators = "",
    section_separators = "",
    disabled_filetypes = { "dashboard", "NvimTree", "Outline" },
  },
  sections = {
    lualine_a = {
      components.mode,
    },
    lualine_b = {},
    lualine_c = {
      components.branch,
      components.filename,
      components.diff,
    },
    lualine_x = {
      components.diagnostics,
      components.treesitter,
      components.lsp,
      components.filetype,
    },
    lualine_y = {},
    lualine_z = {},
  },
})
