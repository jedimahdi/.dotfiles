local u = require("config.utils")

local icons = { error = "", warn = "", info = "", hint = "" }
-- Icon and highlight information for each diagnostic severity.
---@type { icon: string, hl: string }[]
local severity_info = {
  { icon = "E", hl = "DiagnosticSignError" },
  { icon = "W", hl = "DiagnosticSignWarn" },
  { icon = "I", hl = "DiagnosticSignInfo" },
  { icon = "H", hl = "DiagnosticSignHint" },
}

for _, info in ipairs(severity_info) do
  vim.fn.sign_define(info.hl, { text = info.icon, texthl = info.hl })
end

-- Format the diagnostic message to include the `code` value.
---@param diagnostic table
---@return string
local function format_diagnostic(diagnostic)
  local message = diagnostic.message
  local code = diagnostic.code or (diagnostic.user_data and diagnostic.user_data.lsp.code)
  if code then
    message = ("%s (%s)"):format(message, code)
  end
  return message
end

-- Prefix each diagnostic in the floating window with an appropriate icon.
---@param diagnostic table
---@return string #icon as per the diagnostic severity
---@return string #highlight group as per the diagnostic severity
local function prefix_diagnostic(diagnostic)
  local info = severity_info[diagnostic.severity]
  return info.icon .. " ", info.hl
end

-- Global diagnostic configuration.
vim.diagnostic.config({
  underline = false,
  virtual_text = false,
  signs = true,
  severity_sort = true,
  float = {
    header = false,
    source = "always",
    border = "rounded",
    format = format_diagnostic,
    prefix = prefix_diagnostic,
  },
})

-- For all types of diagnostics: `[d`, `]d`
u.map("n", "[d", function()
  vim.diagnostic.goto_prev({
    float = { focusable = false },
  })
end, { desc = "Diagnostic: Goto prev" })
u.map("n", "]d", function()
  vim.diagnostic.goto_next({
    float = { focusable = false },
  })
end, { desc = "Diagnostic: Goto next" })

-- For warning and error diagnostics: `[w`, `]w`
u.map("n", "[w", function()
  vim.diagnostic.goto_prev({
    float = { focusable = false },
    severity = { min = vim.diagnostic.severity.WARN },
  })
end, { desc = "Diagnostic: Goto prev (warning/error)" })
u.map("n", "]w", function()
  vim.diagnostic.goto_next({
    float = { focusable = false },
    severity = { min = vim.diagnostic.severity.WARN },
  })
end, { desc = "Diagnostic: Goto next (warning/error)" })

u.map("n", "<space>l", function()
  vim.diagnostic.open_float({ scope = "line" })
end, { desc = "Show line diagnostics" })
