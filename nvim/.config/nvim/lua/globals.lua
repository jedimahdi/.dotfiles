CONFIG_PATH = vim.fn.stdpath('config')
DATA_PATH = vim.fn.stdpath('data')
CACHE_PATH = vim.fn.stdpath('cache')

O = {
  tsserver = {
    -- @usage can be 'eslint'
    linter = '',
    -- @usage can be 'prettier'
    formatter = 'prettier',
    autoformat = true,
    diagnostics = { virtual_text = { spacing = 0, prefix = '' }, signs = true, underline = true },
  },
  python = {
    linter = '',
    -- @usage can be 'yapf', 'black'
    formatter = '',
    autoformat = false,
    isort = false,
    diagnostics = { virtual_text = { spacing = 0, prefix = '' }, signs = true, underline = true },
    analysis = { type_checking = 'off', auto_search_paths = true, use_library_code_types = true },
  },
  lua = {
    -- @usage can be 'lua-format'
    formatter = 'lua-format',
    autoformat = false,
    diagnostics = { virtual_text = { spacing = 0, prefix = '' }, signs = true, underline = true },
  },
  sh = {
    -- @usage can be 'shellcheck'
    linter = '',
    -- @usage can be 'shfmt'
    formatter = '',
    autoformat = false,
    diagnostics = { virtual_text = { spacing = 0, prefix = '' }, signs = true, underline = true },
  },
}
