local u = require("config.utils")

local cmp = require("cmp")
-- local luasnip = require("luasnip")
local snippy = require("snippy")
local lspkind = require("lspkind")

-- local has_words_before = function()
--   local line, col = unpack(vim.api.nvim_win_get_cursor(0))
--   return col ~= 0 and vim.api.nvim_buf_get_lines(0, line - 1, line, true)[1]:sub(col, col):match("%s") == nil
-- end

lspkind.init()

-- vim.opt.completeopt = { "menu", "menuone", "noselect", "noinsert" }
vim.opt.completeopt = {
  -- show menu even if there is only one match
  "menuone",
  -- do not select any text until the user selects a match from the menu
  "noinsert",
  -- do not select a match in the menu, force the user to select one
  "noselect",
}

cmp.setup({
  snippet = {
    expand = function(args)
      snippy.expand_snippet(args.body)
    end,
  },
  -- window = {
  --   completion = cmp.config.window.bordered(),
  --   documentation = cmp.config.window.bordered(),
  -- },
  preselect = cmp.PreselectMode.None,
  completion = {
    completeopt = "menuone,noinsert,noselect",
    get_trigger_characters = function(trigger_characters)
      return vim.tbl_filter(function(char)
        return char ~= " " and char ~= "\t" and char ~= "\n"
      end, trigger_characters)
    end,
  },
  mapping = {
    ["<C-Space>"] = cmp.mapping(function()
      cmp.complete()
    end, { "i", "s" }),
    ["<CR>"] = cmp.mapping(function(fallback)
      if cmp.get_selected_entry() then
        cmp.confirm()
      else
        fallback()
      end
    end, { "i", "s" }),
    ["<C-e>"] = cmp.mapping({
      i = cmp.mapping.abort(),
      c = cmp.mapping.close(),
    }),
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif snippy.can_expand_or_advance() then
        snippy.expand_or_advance()
        -- elseif has_words_before() then
        --   cmp.complete()
      else
        fallback()
      end
    end, {
      "i",
      "s",
    }),
    ["<S-Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_prev_item()
      elseif snippy.can_jump(-1) then
        snippy.previous()
      else
        fallback()
      end
    end, { "i", "s" }),
  },
  sources = {
    { name = "snippy", priority = 9999, max_item_count = 1 },
    { name = "nvim_lua" },
    { name = "nvim_lsp" },
    { name = "path" },
    { name = "buffer", keyword_length = 5 },
  },
  -- experimental = { ghost_text = true },
  formatting = {
    fields = { "kind", "abbr", "menu" },
    format = function(entry, vim_item)
      local lspkind_icons = {
        Text = "",
        Method = "",
        Function = "",
        Constructor = " ",
        Field = "",
        Variable = "",
        Class = "",
        Interface = "",
        Module = "硫",
        Property = "",
        Unit = " ",
        Value = "",
        Enum = " ",
        Keyword = "ﱃ",
        Snippet = " ",
        Color = " ",
        File = " ",
        Reference = "Ꮢ",
        Folder = " ",
        EnumMember = " ",
        Constant = " ",
        Struct = " ",
        Event = "",
        Operator = "",
        TypeParameter = " ",
      }
      local meta_type = vim_item.kind
      -- load lspkind icons
      vim_item.kind = lspkind_icons[vim_item.kind] .. ""

      vim_item.menu = ({
        buffer = " Buffer",
        nvim_lsp = meta_type,
        path = " Path",
        luasnip = " LuaSnip",
      })[entry.source.name]

      return vim_item
    end,
  },
  -- formatting = {
  --   format = lspkind.cmp_format({
  --     with_text = true,
  --     menu = {
  --       buffer = "[buf]",
  --       nvim_lsp = "[LSP]",
  --       nvim_lua = "[api]",
  --       path = "[path]",
  --       luasnip = "[snip]",
  --       gh_issues = "[issues]",
  --       tn = "[TabNine]",
  --     },
  --   }),
  -- },
})

cmp.setup.cmdline(":", {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = "path" },
  }, {
    { name = "cmdline" },
  }),
})
