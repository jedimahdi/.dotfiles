local cmp = require("cmp")

local u = require("utils")

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
    -- ["<Tab>"] = function(fallback)
    --   if cmp.visible() then
    --     cmp.select_next_item()
    --   elseif vim.fn["vsnip#available"](1) > 0 then
    --     u.input("<Plug>(vsnip-expand-or-jump)")
    --   else
    --     fallback()
    --   end
    -- end,
    ["<Tab>"] = cmp.mapping.select_next_item(),
    ["<S-Tab>"] = cmp.mapping.select_prev_item(),
    ["<C-k>"] = cmp.mapping.confirm({
      behavior = cmp.ConfirmBehavior.Replace,
      select = true,
    }),
  },
  sources = {
    { name = "nvim_lsp" },
    { name = "nvim_lua" },
    { name = "buffer" },
    { name = "vsnip", priority = 9999 },
    { name = "path" },
  },
})
