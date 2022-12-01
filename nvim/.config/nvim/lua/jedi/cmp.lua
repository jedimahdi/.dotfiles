local sumneko_root_path = "/home/mahdi/code/sumneko"
local sumneko_binary = sumneko_root_path .. "/bin/lua-language-server"

local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

local cmp = require("cmp")
local source_mapping = {
  buffer = "[Buffer]",
  nvim_lsp = "[LSP]",
  nvim_lua = "[Lua]",
  path = "[Path]",
}
local lspkind = require("lspkind")
local snippy = require("snippy")

vim.opt.completeopt = {
  "menuone",
  "noinsert",
  "noselect",
}

local border = {
  { "╭", "FoldColumn" },
  { "─", "FoldColumn" },
  { "╮", "FoldColumn" },
  { "│", "FoldColumn" },
  { "╯", "FoldColumn" },
  { "─", "FoldColumn" },
  { "╰", "FoldColumn" },
  { "│", "FoldColumn" },
}

local kind_icons = {
  Text = "",
  Method = "m",
  Function = "",
  Constructor = "",
  Field = "",
  Variable = "",
  Class = "",
  Interface = "",
  Module = "",
  Property = "",
  Unit = "",
  Value = "",
  Enum = "",
  Keyword = "",
  Snippet = "",
  Color = "",
  File = "",
  Reference = "",
  Folder = "",
  EnumMember = "",
  Constant = "",
  Struct = "",
  Event = "",
  Operator = "",
  TypeParameter = "",
}

cmp.setup({
  snippet = {
    expand = function(args)
      require("snippy").expand_snippet(args.body)
    end,
  },
  preselect = cmp.PreselectMode.None,
  mapping = cmp.mapping.preset.insert({
    ["<C-y>"] = cmp.mapping.confirm({ select = true }),
    ["<C-u>"] = cmp.mapping.scroll_docs(-4),
    ["<C-d>"] = cmp.mapping.scroll_docs(4),
    ["<C-Space>"] = cmp.mapping.complete(),
    ["<Tab>"] = cmp.mapping(function(fallback)
      if cmp.visible() then
        cmp.select_next_item()
      elseif snippy.can_expand_or_advance() then
        snippy.expand_or_advance()
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
  }),

  window = {
    completion = {
      border = border,
      scrollbar = "║",
    },
    documentation = {
      border = border,
      scrollbar = "║",
    },
  },

  formatting = {
    fields = { "kind", "abbr", "menu" },
    format = function(entry, vim_item)
      vim_item.kind = string.format("%s", kind_icons[vim_item.kind])
      -- kind menu
      vim_item.menu = ({
        luasnip = "[LuaSnip]",
        nvim_lsp = "[LSP]",
        path = "/",
      })[entry.source.name]
      return vim_item
    end,
  },

  sources = {
    { name = "snippy" },
    { name = "path" },
    { name = "nvim_lsp" },
  },
})
