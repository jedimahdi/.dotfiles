local luaFormat = {
  formatCommand = "lua-format -i --no-keep-simple-function-one-line --column-limit=120 --indent-width=2 --tab-width=2 --spaces-inside-table-braces --double-quote-to-single-quote --no-use-tab --extra-sep-at-table-end",
  formatStdin = true,
}

local lua_arguments = { luaFormat }

local prettier = { formatCommand = "prettier --stdin-filepath ${INPUT}", formatStdin = true }
local tsserver_args = { prettier }

require("lspconfig").efm.setup({
  cmd = { DATA_PATH .. "/lspinstall/efm/efm-langserver" },
  init_options = { documentFormatting = true, codeAction = false },
  filetypes = {
    "lua",
    "javascriptreact",
    "javascript",
    "typescript",
    "typescriptreact",
    "html",
    "css",
    "json",
    "yaml",
  },
  settings = {
    rootMarkers = { ".git/", "package.json" },
    languages = {
      lua = lua_arguments,
      javascript = tsserver_args,
      javascriptreact = tsserver_args,
      typescript = tsserver_args,
      typescriptreact = tsserver_args,
      html = { prettier },
      css = { prettier },
      json = { prettier },
      yaml = { prettier },
    },
  },
})
