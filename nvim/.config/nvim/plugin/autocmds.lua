local api = vim.api

-- highlight on yank
api.nvim_create_autocmd("TextYankPost", {
  callback = function()
    vim.highlight.on_yank({ higroup = "IncSearch", timeout = 500 })
  end,
})

-- terminals
api.nvim_create_autocmd("TermOpen", {
  callback = function()
    -- disable line numbers
    vim.opt_local.number = false
    vim.opt_local.relativenumber = false
    -- always start in insert mode
    vim.cmd("startinsert")
  end,
})
