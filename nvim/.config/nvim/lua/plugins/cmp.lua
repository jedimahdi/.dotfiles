local M = {}

local check_back_space = function()
  local col = vim.fn.col(".") - 1
  return col == 0 or vim.fn.getline("."):sub(col, col):match("%s")
end

M.setup = function()
  local status_ok, cmp = pcall(require, "cmp")
  if not status_ok then
    return
  end

  cmp.setup({
    snippet = {
      expand = function(args)
        vim.fn["vsnip#anonymous"](args.body)
      end,
    },
    sources = {
      { name = "nvim_lsp" },
      { name = "path" },
      { name = "vsnip" },
      { name = "buffer" },
      { name = "nvim_lua" },
    },
    mapping = {
      ["<Tab>"] = cmp.mapping.select_next_item(),
      -- ["<Tab>"] = cmp.mapping(cmp.mapping.select_next_item(), { "i", "s" }),
      -- ["<Tab>"] = function(fallback)
      --   if vim.fn.pumvisible() == 1 then
      --     vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<C-n>", true, true, true), "n")
      --   elseif check_back_space() then
      --     vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<Tab>", true, true, true), "n")
      --   elseif vim.fn["vsnip#available"]() == 1 then
      --     vim.fn.feedkeys(vim.api.nvim_replace_termcodes("<Plug>(vsnip-expand-or-jump)", true, true, true), "")
      --   else
      --     fallback()
      --   end
      -- end,
      ["<S-Tab>"] = cmp.mapping.select_prev_item(),
      ["<C-e>"] = cmp.mapping.close(),
      ["<C-k>"] = cmp.mapping.confirm({
        behavior = cmp.ConfirmBehavior.Replace,
        select = true,
      }),
    },
  })
end

return M
