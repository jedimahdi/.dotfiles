local util = require("formatter.util")
local path = require("mason-core.path")
local mason_bin_dir = path.concat({ vim.fn.stdpath("data"), "mason", "bin" })

local js_formatter = function()
  local formatter = require("formatter.filetypes.javascript").prettierd()
  formatter["exe"] = mason_bin_dir .. "/prettierd"
  return formatter
end

require("formatter").setup({
  logging = true,
  log_level = vim.log.levels.WARN,
  filetype = {
    lua = {
      function()
        local formatter = require("formatter.filetypes.lua").stylua()
        formatter["exe"] = mason_bin_dir .. "/stylua"
        return formatter
      end,
    },
    javascript = {util.copyf(js_formatter)},
    javascriptreact = {util.copyf(js_formatter)},
    typescript = {util.copyf(js_formatter)},
    typescriptreact = {util.copyf(js_formatter)},
    json = {util.copyf(js_formatter)},
    html = {util.copyf(js_formatter)},
    css = {util.copyf(js_formatter)},
    c = {
      function()
        local formatter = require("formatter.filetypes.c").clangformat()
        formatter["exe"] = mason_bin_dir .. "/clang-format"
        return formatter
      end,
    },
    -- ["*"] = {
    --   require("formatter.filetypes.any").remove_trailing_whitespace,
    -- },
  },
})
