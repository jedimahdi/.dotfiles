local has_lir, lir = pcall(require, "lir")
if not has_lir then
  return
end

local has_devicons, devicons = pcall(require, "nvim-web-devicons")
if has_devicons then
  devicons.setup({
    override = {
      lir_folder_icon = {
        icon = "",
        color = "#7ebae4",
        name = "LirFolderNode",
      },
    },
  })
end

local actions = require("lir.actions")
local mark_actions = require("lir.mark.actions")
local clipboard_actions = require("lir.clipboard.actions")

lir.setup({
  show_hidden_files = true,
  devicons = { enable = true },

  float = {
    winblend = 0,
    win_opts = function()
      local width = math.floor(vim.o.columns * 0.7)
      local height = math.floor(vim.o.lines * 0.7)
      return {
        border = {
          "╭",
          "─",
          "╮",
          "│",
          "╯",
          "─",
          "╰",
          "│",
        },
        width = width,
        height = height,
        row = 1,
        col = math.floor((vim.o.columns - width) / 2),
      }
    end,
  },

  mappings = {
    ["<CR>"] = actions.edit,
    ["l"] = actions.edit,
    ["h"] = actions.up,

    ["K"] = actions.mkdir,
    ["N"] = actions.newfile,
    ["R"] = actions.rename,
    ["@"] = actions.cd,
    ["Y"] = actions.yank_path,
    ["D"] = actions.delete,
    ["."] = actions.toggle_show_hidden,
    ["q"] = actions.quit,
    ["<ESC>"] = actions.quit,

    ["J"] = function()
      mark_actions.toggle_mark("n")
      vim.cmd("normal! j")
    end,
    ["C"] = clipboard_actions.copy,
    ["X"] = clipboard_actions.cut,
    ["P"] = clipboard_actions.paste,
  },
})

-- require("lir.git_status").setup({
--   show_ignored = false,
-- })

-- vim.api.nvim_set_keymap("n", "<leader>e", ":edit %:h<CR>", { noremap = true, silent = true })
vim.keymap.set("n", "<leader>e", require("lir.float").toggle)
