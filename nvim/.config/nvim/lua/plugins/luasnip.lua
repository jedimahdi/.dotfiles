local ls = require("luasnip")
local types = require("luasnip.util.types")
local fmt = require("luasnip.extras.fmt").fmt

ls.config.set_config({
  history = true,
  updateevents = "TextChanged,TextChangedI",
})

local snippet = ls.s
local snippet_from_nodes = ls.sn
local t = ls.text_node
local i = ls.insert_node
local f = ls.function_node
local c = ls.choice_node
local d = ls.dynamic_node

local snippets = {}

local same = function(index)
  return f(function(args)
    return args[1]
  end, { index })
end

snippets.all = {
  snippet("simple", c(1, { t("wow, you were right!"), t("shit!!") })),
  -- snippet(
  --   { trig = "div.(%w+)", regTrig = true },
  --   f(function(_, snip)
  --     return '<div class="' .. snip.captures[1] .. '"></div>'
  --   end, {})
  -- ),
}

snippets.typescript = {
  snippet("im", {
    t("import { "),
    i(2, ""),
    t(" } from '"),
    i(1, ""),
    t("';"),
  }),
  snippet("f", {
    t("function "),
    i(1, "name"),
    t("("),
    i(2, "param: type"),
    t("): "),
    i(3, "type"),
    t({ " {", "" }),
    t("  "),
    i(0),
    t({ "", "}" }),
  }),
  snippet("ef", {
    t("export function "),
    i(1, "name"),
    t("("),
    i(2, "param: type"),
    t("): "),
    i(3, "type"),
    t({ " {", "" }),
    t("  "),
    i(0),
    t({ "", "}" }),
  }),
}

ls.snippets = snippets

vim.keymap.set({ "i", "s" }, "<c-j>", function()
  if ls.expand_or_jumpable() then
    ls.expand_or_jump()
  end
end, { silent = true })

vim.keymap.set({ "i", "s" }, "<c-k>", function()
  if ls.jumpable(-1) then
    ls.jump(-1)
  end
end, { silent = true })

vim.keymap.set("i", "<c-l>", function()
  if ls.choice_active() then
    ls.change_choice(1)
  end
end)
