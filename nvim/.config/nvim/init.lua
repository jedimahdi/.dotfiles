local u = require("utils")

local opt = vim.opt

CONFIG_PATH = vim.fn.stdpath("config")
DATA_PATH = vim.fn.stdpath("data")
CACHE_PATH = vim.fn.stdpath("cache")

vim.g.mapleader = ","

opt.completeopt = { "menuone", "noinsert" }
opt.tabstop = 2
opt.shiftwidth = 2
-- opt.epandtab = true
opt.smarttab = true
opt.ignorecase = true
opt.smartcase = true
opt.magic = true
opt.mouse = "a"
opt.pumheight = 10
opt.splitbelow = true
opt.splitright = true
opt.termguicolors = true
opt.undofile = true
opt.updatetime = 300
opt.scrolloff = 5
opt.sidescrolloff = 2
opt.cursorline = false
opt.number = false
opt.relativenumber = false
opt.signcolumn = "yes"
opt.timeoutlen = 800
opt.shortmess:append("cA")
opt.clipboard:append("unnamed") -- vim.opt.clipboard:append("unnamedplus")
opt.directory = CACHE_PATH .. "/swag/"
opt.undodir = CACHE_PATH .. "/undo/"
opt.backupdir = CACHE_PATH .. "/backup/"
opt.viewdir = CACHE_PATH .. "/view/"
opt.spellfile = CACHE_PATH .. "/spell/en.uft-8.add"
opt.backup = false
opt.writebackup = false
opt.undofile = true
opt.swapfile = false
opt.wrap = true
opt.encoding = "utf-8"
opt.showcmd = false
opt.cmdheight = 1
opt.statusline = [[%f %y %m %= %p%% %l:%c]]
opt.laststatus = 0

vim.cmd("autocmd BufEnter * setlocal formatoptions-=cro")

-- initialize global object for config
global = {}

vim.cmd("set background=dark")
vim.cmd("colorscheme onedarker")
-- vim.cmd("colorscheme kanagawa")

u.nmap("<leader><leader>", ":w<CR>")

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

u.nmap("[q", ":cprev<CR>")
u.nmap("]q", ":cnext<CR>")

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

require("tmux")
require("plugins")
require("lsp")
require("commands")
