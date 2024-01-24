local null_ls = require("null-ls")

local sources = {
  null_ls.builtins.formatting.alejandra,
  null_ls.builtins.diagnostics.statix.with({
    diagnostic_config = {
      virtual_text = false,
      underline = false,
    },
  }),
  null_ls.builtins.formatting.prettierd,
  null_ls.builtins.formatting.stylua,
  null_ls.builtins.formatting.clang_format,
  null_ls.builtins.diagnostics.clang_check,
  null_ls.builtins.formatting.shfmt,
  null_ls.builtins.diagnostics.shellcheck,
  null_ls.builtins.code_actions.shellcheck,
  null_ls.builtins.formatting.fourmolu,
  null_ls.builtins.formatting.cabal_fmt,
  null_ls.builtins.formatting.rustfmt,
}

null_ls.setup({
  sources = sources,
  diagnostic_config = {
    underline = false,
  },
})
