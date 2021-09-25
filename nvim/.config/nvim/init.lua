CONFIG_PATH = vim.fn.stdpath("config")
DATA_PATH = vim.fn.stdpath("data")
CACHE_PATH = vim.fn.stdpath("cache")

require("keymappings")
require("plugins")
require("colorscheme")
require("options")
require("lsp")

require("plugins.lspsaga")
require("plugins.telescope")

-- function define_augroups(definitions) -- {{{1
--   -- Create autocommand groups based on the passed definitions
--   --
--   -- The key will be the name of the group, and each definition
--   -- within the group should have:
--   --    1. Trigger
--   --    2. Pattern
--   --    3. Text
--   -- just like how they would normally be defined from Vim itself
--   for group_name, definition in pairs(definitions) do
--     vim.cmd('augroup ' .. group_name)
--     vim.cmd 'autocmd!'
--     for _, def in pairs(definition) do
--       local command = table.concat(vim.tbl_flatten { 'autocmd', def }, ' ')
--       vim.cmd(command)
--     end

--     vim.cmd 'augroup END'
--   end
-- end

-- define_augroups {
--   _general_settings = {
--     { 'TextYankPost', '*', 'lua require(\'vim.highlight\').on_yank({higroup = \'Search\', timeout = 200})' },
--     { 'BufWinEnter', '*', 'setlocal formatoptions-=c formatoptions-=r formatoptions-=o' },
--     { 'BufWinEnter', 'dashboard', 'setlocal cursorline signcolumn=yes cursorcolumn number' },
--     { 'BufRead', '*', 'setlocal formatoptions-=c formatoptions-=r formatoptions-=o' },
--     { 'BufNewFile', '*', 'setlocal formatoptions-=c formatoptions-=r formatoptions-=o' },
--     { 'BufWritePost', 'init.lua', 'source ~/.config/nvim/init.lua' },
--   },
--   _packer_compile = { { 'BufWritePost', 'plugins.lua', 'PackerCompile' } },
-- }

-- local myHover = function(err, method, params, client_id, bufnr, config)
--   print(vim.lsp.util.trim_empty_lines(vim.lsp.util.convert_input_to_markdown_lines(params.contents)))
--   -- print(vim.inspect(params))
-- end
-- vim.lsp.handlers['textDocument/hover'] = myHover
