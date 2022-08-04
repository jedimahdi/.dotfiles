local Remap = require("jedi.keymap")
local nnoremap = Remap.nnoremap

local tmux = require("jedi.tmux")

nnoremap("<C-h>", function ()
  tmux.move('h')
end)

nnoremap("<C-j>", function ()
  tmux.move('j')
end)

nnoremap("<C-k>", function ()
  tmux.move('k')
end)

nnoremap("<C-l>", function ()
  tmux.move('l')
end)
