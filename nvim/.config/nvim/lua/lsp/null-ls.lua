local null_ls = require("null-ls")
local b = null_ls.builtins

local with_root_file = function(builtin, file)
  return builtin.with({
    condition = function(utils)
      return utils.root_has_file(file)
    end,
  })
end

local sources = {
  b.formatting.prettier,
  -- with_root_file(b.formatting.stylua, ".stylua.toml"),
  b.formatting.stylua,
  b.formatting.trim_whitespace.with({ filetypes = { "tmux", "teal", "zsh" } }),
  b.formatting.shfmt,
  -- b.diagnostics.write_good,
  -- b.diagnostics.markdownlint,
  b.diagnostics.shellcheck.with({ diagnostics_format = "#{m} [#{c}]" }),
}

local M = {}
M.setup = function(on_attach)
  null_ls.setup({
    -- debug = true,
    sources = sources,
    on_attach = on_attach,
  })
end

return M
