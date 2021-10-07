local u = require("utils")

local api = vim.api

local tmux_directions = { h = "L", j = "D", k = "U", l = "R" }

local M = {}

local tmux_move = function(direction)
    vim.fn.system("tmux selectp -" .. tmux_directions[direction])
end

M.move = function(direction)
    local current_win = api.nvim_get_current_win()
    vim.cmd("wincmd " .. direction)

    if api.nvim_get_current_win() == current_win then
        tmux_move(direction)
    end
end

u.map("n", "<C-h>", ":lua require'tmux'.move('h')<CR>")
u.map("n", "<C-j>", ":lua require'tmux'.move('j')<CR>")
u.map("n", "<C-k>", ":lua require'tmux'.move('k')<CR>")
u.map("n", "<C-l>", ":lua require'tmux'.move('l')<CR>")

return M
