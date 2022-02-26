require("config.globals")
local u = require("config.utils")

-- initialize global object for config
global = {}

vim.cmd("set background=dark")
vim.g.tokyonight_style = "night"
-- vim.cmd("colorscheme onedarker")
-- vim.cmd("colorscheme kanagawa")

if vim.fn.filereadable(vim.fn.expand("~/.vimrc_background")) then
  vim.g.base16colorspace = 256
  vim.cmd([[source ~/.vimrc_background]])
end

vim.g.mapleader = ","

-- u.nmap("<leader><leader>", ":w<CR>")

u.nmap("<leader>i", ":set cursorline!<CR>")

u.nmap("<BS>", "<C-^>")
u.nmap("<leader>.", "<C-^>")

u.nmap("<Tab>", "%", { noremap = false })
u.xmap("<Tab>", "%", { noremap = false })
u.omap("<Tab>", "%", { noremap = false })

u.nmap("H", "^")
u.omap("H", "^")
u.xmap("H", "^")
u.nmap("L", "$")
u.omap("L", "$")
u.xmap("L", "$")

u.nmap("<leader>x", ":!chmod +x %<CR>")

u.nmap("n", "nzz")
u.nmap("N", "Nzz")

u.xmap(">", ">gv")
u.xmap("<", "<gv")

u.xmap("K", ":move '<-2<CR>gv-gv")
u.xmap("J", ":move '>+1<CR>gv-gv")

u.nmap("<leader>y", '"+y')
u.map("v", "<leader>y", '"+y')
u.nmap("<leader>Y", 'gg"+yG')

u.nmap("<Space>", ":set hlsearch! hlsearch?<CR>")

u.nmap("<leader><space>", [[:%s/\s\+$<cr>]])
u.nmap("<leader><space><space>", [[:%s/\n\{2,}/\r\r/g<cr>]])

u.nmap("[q", ":cprev<CR>")
u.nmap("]q", ":cnext<CR>")

u.nmap("_", ":Ranger<CR>")

u.command("Q", "q")
u.command("W", "w")
u.command("Wq", "wq")
u.command("WQ", "wq")

-- autocommands
-- highlight on yank
vim.cmd('autocmd TextYankPost * silent! lua vim.highlight.on_yank({ higroup = "IncSearch", timeout = 500 })')

-- terminals
-- always start in insert mode
vim.cmd("autocmd TermOpen * startinsert")
-- disable line numbers
vim.cmd("autocmd TermOpen * setlocal nonumber norelativenumber")
-- suppress process exited message
vim.cmd("autocmd TermClose term://*lazygit execute 'bdelete! ' . expand('<abuf>')")

local colors = { black = "#202328", bg = "#202328" }

vim.cmd("hi StatusLine guibg=" .. colors.bg)
vim.cmd([[highlight SpecialKey ctermfg=19 guifg=#333333]])
vim.cmd([[highlight NonText ctermfg=19 guifg=#333333]])

vim.cmd([[highlight MatchParen guibg=none guifg=#555555]])

vim.g.ranger_map_keys = 0

pcall(require, "impatient")

if require("config.first_load")() then
  return
end

vim.cmd([[runtime plugin/astronauta.vim]])

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.g.loaded_netrwSettings = 1
vim.g.loaded_matchit = 1
vim.g.loaded_gzip = 1
vim.g.loaded_tar = 1
vim.g.loaded_tarPlugin = 1
vim.g.loaded_tutor_mode_plugin = 1
vim.g.loaded_zip = 1
vim.g.loaded_zipPlugin = 1

vim.g.border_style = "edge"

require("config")
require("plugins")
require("lsp")
