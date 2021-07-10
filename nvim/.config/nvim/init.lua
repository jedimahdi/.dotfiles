CONFIG_PATH = vim.fn.stdpath 'config'
DATA_PATH = vim.fn.stdpath 'data'
CACHE_PATH = vim.fn.stdpath 'cache'

require 'keymappings'
require 'plugins'
require 'colorscheme'
require 'options'
require 'lsp'

require 'plugins.treesitter'
require 'plugins.galaxyline'
require 'plugins.lspsaga'
