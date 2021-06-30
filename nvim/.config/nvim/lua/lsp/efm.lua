-- Example configuations here: https://github.com/mattn/efm-langserver
-- lua
local lua_arguments = {}

local luaFormat = {
  formatCommand = 'lua-format -i --no-keep-simple-function-one-line --column-limit=120 --indent-width=2 --tab-width=2 --spaces-inside-table-braces --double-quote-to-single-quote --no-use-tab --extra-sep-at-table-end',
  formatStdin = true,
}

local lua_fmt = { formatCommand = 'luafmt --indent-count 2 --line-width 120 --stdin', formatStdin = true }

if O.lua.formatter == 'lua-format' then
  table.insert(lua_arguments, luaFormat)
elseif O.lua.formatter == 'lua-fmt' then
  table.insert(lua_arguments, lua_fmt)
end

-- sh
local sh_arguments = {}

local shfmt = { formatCommand = 'shfmt -ci -s -bn', formatStdin = true }

local shellcheck = {
  LintCommand = 'shellcheck -f gcc -x',
  lintFormats = { '%f:%l:%c: %trror: %m', '%f:%l:%c: %tarning: %m', '%f:%l:%c: %tote: %m' },
}

if O.sh.formatter == 'shfmt' then table.insert(sh_arguments, shfmt) end

if O.sh.linter == 'shellcheck' then table.insert(sh_arguments, shellcheck) end

-- tsserver/web javascript react, vue, json, html, css, yaml
local prettier = { formatCommand = 'prettier --stdin-filepath ${INPUT}', formatStdin = true }
-- You can look for project scope Prettier and Eslint with e.g. vim.fn.glob("node_modules/.bin/prettier") etc. If it is not found revert to global Prettier where needed.
-- local prettier = {formatCommand = "./node_modules/.bin/prettier --stdin-filepath ${INPUT}", formatStdin = true}

local eslint = {
  lintCommand = './node_modules/.bin/eslint -f unix --stdin --stdin-filename ${INPUT}',
  lintIgnoreExitCode = true,
  lintStdin = true,
  lintFormats = { '%f:%l:%c: %m' },
  formatCommand = './node_modules/.bin/eslint --fix-to-stdout --stdin --stdin-filename=${INPUT}',
  formatStdin = true,
}

local tsserver_args = {}

if O.tsserver.formatter == 'prettier' then table.insert(tsserver_args, prettier) end

if O.tsserver.linter == 'eslint' then table.insert(tsserver_args, eslint) end

require'lspconfig'.efm.setup {
  -- init_options = {initializationOptions},
  cmd = { DATA_PATH .. '/lspinstall/efm/efm-langserver' },
  init_options = { documentFormatting = true, codeAction = false },
  filetypes = {
    'lua', 'javascriptreact', 'javascript', 'typescript', 'typescriptreact', 'sh', 'html', 'css', 'json', 'yaml', 'vue',
  },
  settings = {
    rootMarkers = { '.git/' },
    languages = {
      lua = lua_arguments,
      sh = sh_arguments,
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
}

-- Also find way to toggle format on save
-- maybe this will help: https://superuser.com/questions/439078/how-to-disable-autocmd-or-augroup-in-vim
