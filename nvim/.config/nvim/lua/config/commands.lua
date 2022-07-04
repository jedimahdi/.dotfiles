local u = require("config.utils")

local api = vim.api

-- make global to make ex commands easier
_G.inspect = function(...)
  print(vim.inspect(...))
end

local commands = {}

-- open file manager in floating terminal window and edit selected file(s)
-- requires file manager to output paths to stdout, split w/ newlines
commands.file_manager = function(command, edit_command)
  edit_command = edit_command or "edit"
  local winnr, win_bufnr = u.make_floating_window()

  local map_edit_command = function(keys, new_command)
    vim.keymap.set("t", keys, function()
      edit_command = new_command
      u.input("<CR>")
    end, {
      buffer = win_bufnr,
    })
  end

  map_edit_command("<C-v>", "vsplit")
  map_edit_command("<C-s>", "split")
  map_edit_command("<C-t>", "tabedit")

  vim.fn.termopen(command, {
    on_exit = function()
      local output = api.nvim_buf_get_lines(win_bufnr, 0, -1, false)
      api.nvim_win_close(winnr, true)

      local files = vim.tbl_filter(function(f)
        return f ~= "" and u.is_file(f)
      end, output)
      vim.tbl_map(function(file)
        vim.cmd(table.concat({ edit_command, file }, " "))
      end, files)

      api.nvim_buf_delete(win_bufnr, { force = true })
    end,
    on_stderr = function(_, err)
      assert(err[1] == "", err[1])
    end,
  })
end

api.nvim_create_user_command("Vifm", function(opts)
  local dir = opts.args ~= "" and opts.args or vim.fn.expand("%:p:h")
  commands.file_manager(table.concat({ "vifm", "--choose-files", "-", dir }, " "))
end, {
  nargs = "?",
  complete = "dir",
})
u.nmap("-", ":Vifm<CR>")

-- lazygit
u.command("Lazygit", "tabnew | term lazygit")
u.nmap("<Leader>gg", ":Lazygit<CR>")

api.nvim_create_autocmd("TermClose", {
  pattern = "term://*lazygit",
  callback = function()
    -- check buffers for changes
    vim.cmd("checktime")

    local bufnr = tonumber(vim.fn.expand("<abuf>"))
    if api.nvim_buf_is_loaded(bufnr) then
      -- suppress process exited messages
      api.nvim_buf_delete(tonumber(bufnr), { force = true })
    end
  end,
})

-- misc
-- reset treesitter and lsp diagnostics
u.command("R", "w | :e")

return commands
