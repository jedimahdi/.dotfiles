local u = require("utils")

CONFIG_PATH = vim.fn.stdpath("config")
DATA_PATH = vim.fn.stdpath("data")
CACHE_PATH = vim.fn.stdpath("cache")

vim.g.mapleader = ","

vim.opt.completeopt = { "menuone", "noinsert" }
vim.opt.tabstop = 2
vim.opt.shiftwidth = 2
vim.opt.expandtab = true
vim.opt.smarttab = true
vim.opt.ignorecase = true
vim.opt.smartcase = true
vim.opt.magic = true
vim.opt.mouse = "a"
vim.opt.pumheight = 10
vim.opt.splitbelow = true
vim.opt.splitright = true
vim.opt.termguicolors = true
vim.opt.undofile = true
vim.opt.updatetime = 300
vim.opt.scrolloff = 5
vim.opt.sidescrolloff = 2
vim.opt.cursorline = false
vim.opt.number = false
vim.opt.relativenumber = false
vim.opt.signcolumn = "yes"
vim.opt.timeoutlen = 800
vim.opt.shortmess:append("cA")
vim.opt.clipboard:append("unnamed") -- vim.opt.clipboard:append("unnamedplus")
vim.opt.directory = CACHE_PATH .. "/swag/"
vim.opt.undodir = CACHE_PATH .. "/undo/"
vim.opt.backupdir = CACHE_PATH .. "/backup/"
vim.opt.viewdir = CACHE_PATH .. "/view/"
vim.opt.spellfile = CACHE_PATH .. "/spell/en.uft-8.add"
vim.opt.backup = false
vim.opt.writebackup = false
vim.opt.undofile = true
vim.opt.swapfile = false
vim.opt.wrap = true
vim.opt.encoding = "utf-8"
vim.opt.showcmd = false
vim.opt.statusline = [[%f %y %m %= %p%% %l:%c]]

vim.cmd("autocmd BufEnter * setlocal formatoptions-=cro")

-- initialize global object for config
global = {}

vim.cmd("set background=dark")
-- vim.cmd("colorscheme onedarker")
vim.cmd("colorscheme kanagawa")

u.nmap("<leader><leader>", ":w<CR>")

u.nmap("<BS>", "<C-^>")
u.nmap("<leader>.", "<C-^>")
u.nmap("<TAB>", ":bnext<CR>")
u.nmap("<TAB>", ":bprevious<CR>")

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

-- always redirect small operations to black hole register
u.nmap("c", '"_c')
u.nmap("C", '"_C')

u.nmap("x", '"_x')
u.nmap("X", '"_X')

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

require("tmux")
require("plugins")
require("lsp")
require("commands")
