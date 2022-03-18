local u = require("config.utils")

--- {{{ General

vim.g.mapleader = " "

u.nmap("<leader>i", ":set cursorline!<CR>")

u.nmap("<leader>x", "<cmd>source %<CR>", { silent = false })

-- Fast switching between last and current file
u.nmap("<BS>", "<C-^>")
u.map("n", "<leader><leader>", "<Cmd>buffer#<CR>")

u.nmap("<Tab>", "%", { noremap = false })
u.xmap("<Tab>", "%", { noremap = false })
u.omap("<Tab>", "%", { noremap = false })

-- Jump to start (`^`) and end (`$`) of line using the home row keys.
u.map({ "n", "x" }, "H", "^")
u.map({ "n", "x" }, "L", "$")

u.nmap("<leader>y", '"+y')
u.map("v", "<leader>y", '"+y')
u.nmap("<leader>Y", 'gg"+yG')

u.map("n", "<leader>rr", function()
  local filetype = vim.bo.filetype
  if filetype == "lua" or filetype == "vim" then
    vim.cmd("source %")
  end
end)

u.nmap("<leader>nh", ":set hlsearch! hlsearch?<CR>")

-- u.nmap("<leader><space>", [[:%s/\s\+$<cr>]])
-- u.nmap("<leader><space><space>", [[:%s/\n\{2,}/\r\r/g<cr>]])
--}}}

-- Search {{{
-- The direction of `n` and `N` depends on whether `/` or `?` was used for
-- searching forward or backward respectively.
--
-- This will make sure that `n` will always search forward and `N` backward.
-- https://github.com/mhinz/vim-galore#saner-behavior-of-n-and-N
-- FIXME: this does not open the folds by default as explained in `:h 'foldopen'`
-- adding 'zv' at the end causes the highlighting to lose
u.map("n", "n", "'Nn'[v:searchforward] . 'zv'", { expr = true })
u.map("n", "N", "'nN'[v:searchforward] . 'zv'", { expr = true })

-- Don't move the cursor to the next match
-- FIXME: if the cursor is not at the start of the word, it is not highlighted
u.map("n", "*", "*``")
u.map("n", "#", "#``")

-- Use `cn/cN` to change the word under cursor or visually selected text and then
-- repeat using . (dot) n - 1 times. We can use `n/N` to skip some replacements.
--
-- Source: http://www.kevinli.co/posts/2017-01-19-multiple-cursors-in-500-bytes-of-vimscript/
--
--                     ┌ populate search register with word under cursor
--                     │
--                     │┌ get back to where we were
--                     ││
--                     ││ ┌ change next occurrence of last used search pattern
--                     │├┐├─┐
u.map("n", "cn", "*``cgn")
u.map("n", "cN", "*``cgN")

-- Similarly in Visual mode
-- vim.g.mc = dm.escape [[y/\V<C-r>=escape(@", '/')<CR><CR>]]
-- u.map("x", "cn", [[g:mc . "``cgn"]], { expr = true })
-- u.map("x", "cN", [[g:mc . "``cgN"]], { expr = true })

-- Substitute the word on cursor or visually selected text globally
u.map("n", "<Leader>su", [[:%s/\<<C-r><C-w>\>//g<Left><Left>]])
u.map("x", "<leader>su", [["zy:%s/\<<C-r><C-o>"\>//g<Left><Left>]])

-- }}}

-- quickfix List {{{

local function quickfix_navigation(cmd, reset)
  -- `v:count1` because we don't want lua to complain.
  for _ = 1, vim.v.count1 do
    local ok, err = pcall(vim.cmd, cmd)
    if not ok then
      -- No more items in the quickfix list; wrap around the edge
      --
      -- Reference: "Vim(cnext):E553: No more items"
      if err:match("^Vim%(%a+%):E553:") then
        vim.cmd(reset)
      end
    end
  end
end

local quickfix_mappings = {
  { lhs = "]q", cmd = "cnext", reset = "cfirst" },
  { lhs = "[q", cmd = "cprev", reset = "clast" },
  { lhs = "]l", cmd = "lnext", reset = "lfirst" },
  { lhs = "[l", cmd = "lprev", reset = "llast" },
}

for _, info in ipairs(quickfix_mappings) do
  u.map("n", info.lhs, function()
    quickfix_navigation(info.cmd, info.reset)
  end)
end

u.map("n", "]Q", "<Cmd>clast<CR>")
u.map("n", "[Q", "<Cmd>cfirst<CR>")

u.map("n", "]L", "<Cmd>llast<CR>")
u.map("n", "[L", "<Cmd>lfirst<CR>")

-- Close location list or quickfix list if they are present,
-- Source: https://superuser.com/q/355325/736190
u.map("n", "<leader>qx", "<Cmd>windo lclose <bar> cclose<CR>")

--- }}}

-- Shortcuts for faster save and quit {{{
--
-- '<leader>w': Save only when the buffer is updated
-- '<leader>q': Save the file if modified, and quit
-- 'Q': Save all the modified buffers and exit vim
-- u.map("n", "<leader>w", "<Cmd>silent update<CR>")
-- u.map("n", "<leader>q", "<Cmd>silent xit<CR>")
u.map("n", "Q", "<Cmd>xall<CR>")

-- }}}

-- Command-Line {{{
-- Make <C-p>/<C-n> as smart as <Up>/<Down> {{{
--
-- This will either recall older/recent command-line from history, whose
-- beginning matches the current command-line or move through the wildmenu
-- completion.
-- }}}
u.map("c", "<C-p>", "wildmenumode() ? '<C-p>' : '<Up>'", { expr = true })
u.map("c", "<C-n>", "wildmenumode() ? '<C-n>' : '<Down>'", { expr = true })

---Navigate search without leaving incremental mode.
---@param key string
---@param fallback string
local function navigate_search(key, fallback)
  local cmdtype = vim.fn.getcmdtype()
  if cmdtype == "/" or cmdtype == "?" then
    return vim.fn.getcmdline() == "" and "<Up>" or key
  end
  return fallback
end

-- Move between matches without leaving incremental search {{{
--
-- By default, when you search for a pattern, `<C-g>` and `<C-t>` allow you
-- to cycle through all the matches, without leaving the command-line. We remap
-- these commands to `<Tab>` and `<S-Tab>`.
--
-- For an empty command-line, pressing either of the two keys will populate the
-- command-line with the last searched pattern.
--
-- Note dependency on `'wildcharm'` being set to `<C-z>` in order for this to work.
-- }}}
u.map("c", "<Tab>", function()
  return navigate_search("<C-g>", "<C-z>")
end, { expr = true })
u.map("c", "<S-Tab>", function()
  return navigate_search("<C-t>", "<S-Tab>")
end, { expr = true })

u.map("c", "<C-a>", "<Home>")
u.map("c", "<C-e>", "<End>")

u.map("c", "<C-f>", "<Right>", { remap = true })
u.map("c", "<C-b>", "<Left>", { remap = true })

-- Make <Left>/<Right> move the cursor instead of selecting a different match
-- in the wildmenu. See :h 'wildmenu'
-- u.map("c", "<Left>", "<Space><BS><Left>")
-- u.map("c", "<Right>", "<Space><BS><Right>")

-- }}}

-- Registers {{{

-- Yank from current cursor position to the end of the line. Make it consistent
-- with the behavior of 'C', 'D'
u.nmap("Y", "y$")

-- Automatically jump to the end of text on yank and paste
-- `] (backticks) to the last character of previously changed or yanked text
u.nmap("p", "p`]")
u.xmap("y", "y`]")

-- Delete without overriding the paste register

-- Delete without overriding the paste register
u.map({ "n", "x" }, "<leader>d", '"_d')

-- Prevent 'x/X' and 'c/C' from overriding what's in the clipboard
u.map({ "n", "x" }, "x", '"_x')
u.map({ "n", "x" }, "X", '"_X')
u.map({ "n", "x" }, "c", '"_c')
u.map({ "n", "x" }, "C", '"_C')
--- }}}

-- Visual {{{

u.map("x", "/", function()
  -- If we've selected only 1 line, we probably don't want to look for a pattern;
  -- instead, we just want to extend the selection.
  if vim.fn.line("v") == vim.fn.line(".") then
    return "/"
  end
  return [[<C-\><C-n>/\%V]]
end, { expr = true })

-- Repeat last edit on all the visually selected lines with dot.
u.map("x", ".", ":normal! .<CR>")

-- Search for visually selected text using '*' and '#'
-- https://vim.fandom.com/wiki/Search_for_visually_selected_text#Simple
u.map("x", "*", [[y/\V<C-R>=escape(@",'/\')<CR><CR>]])
u.map("x", "#", [[y?\V<C-R>=escape(@",'/?')<CR><CR>]])

-- Capital JK move code lines/blocks up & down (only in visual mode)
u.map("x", "J", [[:move '>+1<CR>gv=gv]])
u.map("x", "K", [[:move '<-2<CR>gv=gv]])

-- Visual indentation goes back to same selection
u.xmap("<", "<gv")
u.xmap(">", ">gv")

-- Repeat last macro on all the visually selected lines with `@{reg}`.
u.map("x", "@", [[:<C-U>execute ":* normal @".getcharstr()<CR>]], {
  silent = true,
})

-- Make blockwise Visual mode, especially Visual-block Inserting/Appending,
-- more useful.
--
-- v_b_I = Visual-block Insert (`:h v_b_I`)
-- v_b_A = Visual-block Append (`:h v_b_A`)
--
--   > Make |v_b_I| and |v_b_A| available in all kinds of Visual mode.
--   > Adjust the selected area to be intuitive before doing blockwise insertion.
--
-- Source: https://github.com/kana/vim-niceblock/blob/master/doc/niceblock.txt
local niceblock_keys = {
  -- Terminal code `^V` because that's what `nvim_get_mode` returns
  -- for visual-block mode (`:h i_CTRL_V`) ──┐
  --                                         │
  ["I"] = { v = "<C-V>I", V = "<C-V>^o^I", [""] = "I" },
  ["A"] = { v = "<C-V>A", V = "<C-V>0o$A", [""] = "A" },
  ["gI"] = { v = "<C-V>0I", V = "<C-V>0o$I", [""] = "0I" },
}

---@param key string
---@return string
local function niceblock(key)
  local mode = vim.api.nvim_get_mode().mode
  return niceblock_keys[key][mode]
end

-- Like |v_b_I|, but:
--
--   * It's available in all kinds of Visual mode.
--   * It adjusts the selected area to get intuitive result after blockwise
--     insertion if the current mode is not blockwise.
--   * In linewise Visual mode, text is inserted before the first non-blank column.
u.map("x", "I", function()
  return niceblock("I")
end, { expr = true })

-- Like |v_I|, but it's corresponding to |v_b_A| instead of |v_b_I|.
u.map("x", "A", function()
  return niceblock("A")
end, { expr = true })

-- Like |v_I|, but it behaves like |gI| in Normal mode. Text is always inserted
-- before the first column.
u.map("x", "gI", function()
  return niceblock("gI")
end, { expr = true })

-- }}}

--- {{{ quit commands
u.command("Q", "q")
u.command("W", "w")
u.command("Wq", "wq")
u.command("WQ", "wq")
--}}}
