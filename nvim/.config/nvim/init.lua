require("jedi.set")
require("jedi.packer")
require("jedi.mappings")
require("jedi.telescope")
require("jedi.lir")
require("jedi.colorscheme")
require("jedi.treesitter")
require("jedi.lsp")
require("jedi.cmp")
require("jedi.harpoon")
require("jedi.tmux")

-- Turn on status information
-- require("fidget").setup()

require("Comment").setup()
require("nvim-autopairs").setup({})

-- Snippet
require("snippy").setup({
  mappings = {
    is = {
      ["<C-j>"] = "expand_or_advance",
      ["<C-k>"] = "previous",
    },
  },
  scopes = {
    typescriptreact = { "typescript" },
  },
})

local function find_module_name(lines)
  for _, line in ipairs(lines) do
    if string.find(line, "module") then
      return string.sub(line, string.find(line, "%u%S*"))
    end
  end

  return nil
end

local function run_code()
  -- local bufnr = vim.api.nvim_get_current_buf()
  -- local ns = vim.api.nvim_create_namespace("trolling")
  -- local x = vim.api.nvim_buf_set_extmark(bufnr, ns, 0, 2, { virt_text = { "test worlds" } })
  -- print(bufnr)

  local api = vim.api

  local bnr = vim.fn.bufnr("%")

  local lines = api.nvim_buf_get_lines(bnr, 0, -1, false)

  local module_name = find_module_name(lines)

  if module_name == nil then
    return
  end

  -- local ns_id = api.nvim_create_namespace("demo")
  --
  -- local line_num = 5
  -- local col_num = 5
  --
  -- local opts = {
  --   end_line = 10,
  --   id = 1,
  --   virt_text = { { "demo", "IncSearch" } },
  --   virt_text_pos = "eol",
  --   -- virt_text_win_col = 20,
  -- }
  --
  -- local mark_id = api.nvim_buf_set_extmark(bnr, ns_id, line_num, col_num, opts)
end

-- vim.keymap.set("n", "<leader>c", run_code)

local augroup = vim.api.nvim_create_augroup
local autocmd = vim.api.nvim_create_autocmd

local yank_group = augroup("HighlightYank", {})
autocmd("TextYankPost", {
  group = yank_group,
  pattern = "*",
  callback = function()
    vim.highlight.on_yank({
      higroup = "IncSearch",
      timeout = 100,
    })
  end,
})
