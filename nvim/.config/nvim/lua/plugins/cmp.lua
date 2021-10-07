local cmp = require("cmp")

cmp.setup({
  snippet = {
    expand = function(args)
      vim.fn["vsnip#anonymous"](args.body)
    end,
    get_trigger_characters = function(trigger_characters)
      return vim.tbl_filter(function(char)
        return char ~= " " and char ~= "\t"
      end, trigger_characters)
    end,
  },
  mapping = {
    ["<C-p>"] = cmp.mapping.select_prev_item(),
    ["<C-n>"] = cmp.mapping.select_next_item(),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<C-e>"] = cmp.mapping.close(),
    ["<Tab>"] = cmp.mapping.select_next_item(),
    ["<S-Tab>"] = cmp.mapping.select_prev_item(),
    ["<C-k>"] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    }),
    -- ["<Tab>"] = function(fallback)
    --   if vim.fn.pumvisible() == 1 then
    --     u.input("<C-n>", "n")
    --   elseif vim.fn["vsnip#available"](1) > 0 then
    --     u.input("<Plug>(vsnip-expand-or-jump)")
    --   else
    --     fallback()
    --   end
    -- end,
    -- ["<S-Tab>"] = function(fallback)
    --   if vim.fn.pumvisible() == 1 then
    --     u.input("<C-p>", "n")
    --   elseif vim.fn["vsnip#jumpable"](-1) > 0 then
    --     u.input("<Plug>(vsnip-jump-prev)")
    --   else
    --     fallback()
    --   end
    -- end,
  },
  sources = {
    { name = "nvim_lsp" },
    { name = "nvim_lua" },
    { name = "buffer" },
    { name = "vsnip" },
    { name = "path" },
  },
})
