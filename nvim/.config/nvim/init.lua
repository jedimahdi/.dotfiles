-- install packer on first load
if require("config.first_load")() then
  return
end

local present, impatient = pcall(require, "impatient")

if present then
  impatient.enable_profile()
end

require("config.globals")

-- initialize global object for config
global = {}

if vim.fn.filereadable(vim.fn.expand("~/.vimrc_background")) then
  vim.g.base16colorspace = 256
  vim.cmd([[source ~/.vimrc_background]])
end

local colors = { black = "#202328", bg = "#202328" }

vim.cmd("hi StatusLine guibg=" .. colors.bg)
vim.cmd([[highlight SpecialKey ctermfg=19 guifg=#333333]])
vim.cmd([[highlight NonText ctermfg=19 guifg=#333333]])

-- vim.cmd([[highlight MatchParen guibg=none guifg=#555555]])

vim.g.ranger_map_keys = 0

vim.cmd([[runtime plugin/astronauta.vim]])

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.g.loaded_netrwSettings = 1
-- vim.g.loaded_matchit = 1
vim.g.loaded_gzip = 1
vim.g.loaded_tar = 1
vim.g.loaded_tarPlugin = 1
vim.g.loaded_tutor_mode_plugin = 1
vim.g.loaded_zip = 1
vim.g.loaded_zipPlugin = 1

vim.g.border_style = "edge"

vim.cmd("autocmd BufEnter * setlocal formatoptions-=cro")

require("config")
require("plugins")
require("lsp")
