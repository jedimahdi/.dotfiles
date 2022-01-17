local api = vim.api

local get_map_options = function(custom_options)
  local options = { noremap = true, silent = true }
  if custom_options then
    options = vim.tbl_extend("force", options, custom_options)
  end
  return options
end

local M = {}

M.map = function(mode, target, source, opts)
  api.nvim_set_keymap(mode, target, source, get_map_options(opts))
end

for _, mode in ipairs({ "n", "o", "i", "x", "t" }) do
  M[mode .. "map"] = function(...)
    M.map(mode, ...)
  end
end

M.buf_map = function(bufnr, mode, target, source, opts)
  api.nvim_buf_set_keymap(bufnr or 0, mode, target, source, get_map_options(opts))
end

M.for_each = function(tbl, cb)
  for _, v in ipairs(tbl) do
    cb(v)
  end
end

M.replace = function(str, original, replacement)
  local found, found_end = string.find(str, original, nil, true)
  if not found then
    return
  end

  if str == original then
    return replacement
  end

  local first_half = string.sub(str, 0, found - 1)
  local second_half = string.sub(str, found_end + 1)

  return first_half .. replacement .. second_half
end

_G.inspect = function(...)
  print(vim.inspect(...))
end

M.command = function(name, fn)
  vim.cmd(string.format("command! %s %s", name, fn))
end

M.lua_command = function(name, fn)
  M.command(name, "lua " .. fn)
end

M.augroup = function(name, event, fn, ft)
  api.nvim_exec(
    string.format(
      [[
    augroup %s
        autocmd!
        autocmd %s %s %s
    augroup END
    ]],
      name,
      event,
      ft or "*",
      fn
    ),
    false
  )
end

M.t = function(str)
  return api.nvim_replace_termcodes(str, true, true, true)
end

M.input = function(keys, mode)
  api.nvim_feedkeys(M.t(keys), mode or "m", true)
end

M.buf_augroup = function(name, event, fn)
  api.nvim_exec(
    string.format(
      [[
    augroup %s
        autocmd! * <buffer>
        autocmd %s <buffer> %s
    augroup END
    ]],
      name,
      event,
      fn
    ),
    false
  )
end

M.warn = function(msg)
  api.nvim_echo({ { msg, "WarningMsg" } }, true, {})
end

M.is_file = function(path)
  if path == "" then
    return false
  end

  local stat = vim.loop.fs_stat(path)
  return stat and stat.type == "file"
end

M.make_floating_window = function(custom_window_config, height_ratio, width_ratio)
  height_ratio = height_ratio or 0.8
  width_ratio = width_ratio or 0.8

  local height = math.ceil(vim.opt.lines:get() * height_ratio)
  local width = math.ceil(vim.opt.columns:get() * width_ratio)
  local window_config = {
    relative = "editor",
    style = "minimal",
    border = "double",
    width = width,
    height = height,
    row = width / 2,
    col = height / 2,
  }
  window_config = vim.tbl_extend("force", window_config, custom_window_config or {})

  local bufnr = api.nvim_create_buf(false, true)
  local winnr = api.nvim_open_win(bufnr, true, window_config)
  return winnr, bufnr
end

M.get_system_output = function(cmd)
  return vim.split(vim.fn.system(cmd), "\n")
end

return M