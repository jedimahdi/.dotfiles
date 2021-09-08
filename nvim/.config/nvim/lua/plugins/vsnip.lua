local M = {}

M.setup = function()
  vim.g.vsnip_snippet_dir = vim.fn.stdpath("config") .. "/snippets"

  -- vim.cmd([[
  --   imap <expr> <C-j>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-j>'
  --   smap <expr> <C-j>   vsnip#available(1)  ? '<Plug>(vsnip-expand-or-jump)' : '<C-j>'
  -- ]])
end

return M
