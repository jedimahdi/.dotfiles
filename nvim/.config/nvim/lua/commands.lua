local u = require("utils")

local api = vim.api

local for_each_buffer = function(cb, force)
  u.for_each(vim.fn.getbufinfo({ buflisted = true }), function(b)
    if b.changed == 0 and not force then
      cb(b)
    end
  end)
end

local commands = {}

commands.bonly = function(bufnr)
  bufnr = bufnr or api.nvim_get_current_buf()
  for_each_buffer(function(b)
    if b.bufnr ~= bufnr then
      vim.cmd("silent! bdelete " .. b.bufnr)
    end
  end)
end

u.lua_command("Bonly", "global.commands.bonly()")

commands.bwipeall = function()
  for_each_buffer(function(b)
    vim.cmd("silent! bdelete " .. b.bufnr)
  end)
end

u.lua_command("Bwipeall", "global.commands.bwipeall()")

commands.wwipeall = function()
  local win = api.nvim_get_current_win()
  u.for_each(vim.fn.getwininfo(), function(w)
    if w.winid ~= win then
      if w.loclist == 1 then
        vim.cmd("lclose")
      elseif w.quickfix == 1 then
        vim.cmd("cclose")
      else
        vim.cmd(w.winnr .. " close")
      end
    end
  end)
end

u.lua_command("Wwipeall", "global.commands.wwipeall()")

commands.bdelete = function(bufnr)
  local win = api.nvim_get_current_win()
  bufnr = bufnr or api.nvim_win_get_buf(win)

  local target
  local previous = vim.fn.bufnr("#")
  if previous ~= -1 and previous ~= bufnr and vim.fn.buflisted(previous) == 1 then
    target = previous
  end

  if not target then
    for_each_buffer(function(b)
      if not target and b.bufnr ~= bufnr then
        target = b.bufnr
      end
    end)
  end

  if not target then
    target = api.nvim_create_buf(false, false)
  end

  local windows = vim.fn.getbufinfo(bufnr)[1].windows
  u.for_each(windows, function(w)
    api.nvim_win_set_buf(w, target)
  end)

  vim.cmd("silent! bdelete " .. bufnr)
end

u.lua_command("Bdelete", "global.commands.bdelete()")
u.map("n", "<Leader>cc", ":Bdelete<CR>")

commands.vsplit = function(args)
  if not args then
    vim.cmd("vsplit")
    return
  end

  local edit_in_win = function(winnr)
    vim.cmd(winnr .. "windo edit " .. args)
  end

  local current = vim.fn.winnr()
  local right_split = vim.fn.winnr("l")
  local left_split = vim.fn.winnr("h")
  if left_split < current then
    edit_in_win(left_split)
    return
  end
  if right_split > current then
    edit_in_win(right_split)
    return
  end

  vim.cmd("vsplit " .. args)
end

vim.cmd("command! -complete=file -nargs=* Vsplit lua global.commands.vsplit(<f-args>)")
u.command("VsplitLast", "Vsplit #")
u.map("n", "<Leader>v", ":VsplitLast<CR>")

commands.save_on_cr = function()
  return vim.bo.buftype ~= "" and u.t("<CR>") or u.t(":write<CR>")
end

u.map("n", "<CR>", "v:lua.global.commands.save_on_cr()", { expr = true })

commands.yank_highlight = function()
  vim.highlight.on_yank({ higroup = "IncSearch", timeout = 500 })
end

u.augroup("YankHighlight", "TextYankPost", "lua global.commands.yank_highlight()")

commands.edit_test_file = function(cmd, post)
  cmd = cmd or "edit"

  local done = function(file)
    vim.cmd(cmd .. " " .. file)
    if post then
      post()
    end
  end

  local root, ft = vim.fn.expand("%:t:r"), vim.bo.filetype
  -- escape potentially conflicting characters in filename
  root = root:gsub("%-", "%%-")
  root = root:gsub("%.", "%%.")

  local patterns = {}
  if ft == "lua" then
    table.insert(patterns, "_spec")
  elseif ft == "typescript" or ft == "typescriptreact" then
    table.insert(patterns, "%.test")
    table.insert(patterns, "%.spec")
  end

  local final_patterns = {}
  for _, pattern in ipairs(patterns) do
    -- go from test file to non-test file
    if root:match(pattern) then
      pattern = u.replace(root, pattern, "")
    else
      pattern = root .. pattern
    end
    -- make sure extension matches
    pattern = pattern .. "%." .. vim.fn.expand("%:e") .. "$"
    table.insert(final_patterns, pattern)
  end

  -- check buffers first
  for _, b in ipairs(vim.fn.getbufinfo({ buflisted = 1 })) do
    for _, pattern in ipairs(final_patterns) do
      if b.name:match(pattern) then
        vim.cmd(cmd .. " #" .. b.bufnr)
        return
      end
    end
  end

  local scandir = function(path, depth, next)
    require("plenary.scandir").scan_dir_async(path, {
      depth = depth,
      search_pattern = final_patterns,
      on_exit = vim.schedule_wrap(function(found)
        if found[1] then
          done(found[1])
          return
        end

        assert(next, "test file not found")
        next()
      end),
    })
  end

  -- check same dir files first, then cwd
  scandir(vim.fn.expand("%:p:h"), 1, function()
    scandir(vim.fn.getcwd(), 5)
  end)
end

vim.cmd("command! -complete=command -nargs=* TestFile lua global.commands.edit_test_file(<f-args>)")
u.map("n", "<Leader>tv", ":TestFile Vsplit<CR>")

commands.terminal = {
  on_open = function()
    -- start in insert mode and turn off line numbers
    vim.cmd("startinsert")
    vim.cmd("setlocal nonumber norelativenumber")
  end,

  -- suppress exit code message
  on_close = function()
    if vim.bo.filetype ~= "nnn" then
      vim.api.nvim_input("<CR>")
    end
  end,
}

u.augroup("OnTermOpen", "TermOpen", "lua global.commands.terminal.on_open()")
u.augroup("OnTermClose", "TermClose", "lua global.commands.terminal.on_close()")

u.command(
  "WipeReg",
  [[for r in split('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789/-"', '\zs')| silent! call setreg(r, []) | endfor]]
)
u.augroup("WipeRegisters", "VimEnter", "WipeReg")

-- reset LSP diagnostics and treesitter
u.command("R", "w | :e")

-- delete current file and buffer
u.command("Remove", "call delete(expand('%')) | Bdelete")

_G.global.commands = commands

return commands
