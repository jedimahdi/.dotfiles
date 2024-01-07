local Remap = require("jedi.keymap")

local nnoremap = Remap.nnoremap
local vnoremap = Remap.vnoremap
local inoremap = Remap.inoremap
local xnoremap = Remap.xnoremap
local nmap = Remap.nmap

vim.keymap.set({ "n", "v" }, "<Space>", "<Nop>", { silent = true })

inoremap("<C-H>", "<C-w>")

vim.keymap.set({ "n", "x" }, "c", '"_c')
vim.keymap.set("n", "cc", '"_cc')
vim.keymap.set("n", "C", '"_C')

vim.keymap.set({ "n", "x" }, "x", '"_x')

-- Go to the beginning and end of current line in insert mode quickly
vim.keymap.set("i", "<C-A>", "<HOME>")
vim.keymap.set("i", "<C-E>", "<END>")

-- Go to beginning of command in command-line mode
vim.keymap.set("c", "<C-A>", "<HOME>")

-- Delete the character to the right of the cursor
vim.keymap.set("i", "<C-D>", "<DEL>")

-- nnoremap("<leader>e", ":Ex<CR>")
nnoremap("<leader>u", ":UndotreeShow<CR>")
vnoremap("K", ":m '<-2<CR>gv=gv")

vnoremap("J", ":m '>+1<CR>gv=gv")

nnoremap("Y", "yg$")
nnoremap("n", "nzzzv")
nnoremap("N", "Nzzzv")
nnoremap("J", "mzJ`z")
-- nnoremap("<C-d>", "<C-d>zz")
-- nnoremap("<C-u>", "<C-u>zz")

-- greatest remap ever
xnoremap("<leader>p", '"_dP')

-- next greatest remap ever : asbjornHaland
nnoremap("<leader>y", '"+y')
vnoremap("<leader>y", '"+y')
nmap("<leader>Y", '"+Y')

nnoremap("<leader>d", '"_d')
vnoremap("<leader>d", '"_d')

vnoremap("<leader>d", '"_d')

-- This is going to get me cancelled
inoremap("<C-c>", "<Esc>")

-- nnoremap("Q", "<nop>")
-- nnoremap("<C-f>", "<cmd>silent !tmux neww tmux-sessionizer<CR>")

nnoremap("<C-n>", "<cmd>cnext<CR>zz")
nnoremap("<C-p>", "<cmd>cprev<CR>zz")
nnoremap("<leader>k", "<cmd>lnext<CR>zz")
nnoremap("<leader>j", "<cmd>lprev<CR>zz")

nnoremap("<leader>r", ":%s/\\<<C-r><C-w>\\>/<C-r><C-w>/gI<Left><Left><Left>")
-- nnoremap("<leader>x", "<cmd>!chmod +x %<CR>", { silent = true })

nnoremap("<leader>w", "<cmd>silent update<CR>")
nnoremap("<leader>q", "<cmd>silent xit<CR>")
nnoremap("Q", "<cmd>xall<CR>")
nnoremap("<leader><leader>", "<cmd>buffer#<CR>")

nnoremap("<leader>z", "<cmd>TSPlaygroundToggle<CR>")

-- Format
nnoremap("<leader>p", function()
  vim.lsp.buf.format({
    filter = function(client)
      return client.name == "null-ls"
    end,
  })
end)

nnoremap("<leader>n", "<cmd>nohls<CR>")
