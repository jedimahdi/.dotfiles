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
    -- Youtube: How to set up nice formatting for your sources.
    format = lspkind.cmp_format({
      with_text = true,
      menu = {
        buffer = "[buf]",
        nvim_lsp = "[LSP]",
        nvim_lua = "[api]",
        path = "[path]",
        luasnip = "[snip]",
        gh_issues = "[issues]",
        tn = "[TabNine]",
      },
    }),
  },
})

cmp.setup.cmdline(":", {
  mapping = cmp.mapping.preset.cmdline(),
  sources = cmp.config.sources({
    { name = "path" },
  }, {
    { name = "cmdline" },
  }),
})
