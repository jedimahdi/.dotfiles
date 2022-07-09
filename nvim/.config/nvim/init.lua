-- install packer on first load
if require("config.first_load")() then
  return
end

vim.g.do_filetype_lua = 1
vim.g.did_load_filetypes = 0

vim.g.loaded_perl_provider = 0
vim.g.loaded_ruby_provider = 0

local present, impatient = pcall(require, "impatient")

if present then
  impatient.enable_profile()
end

require("config.globals")

-- initialize global object for config
global = {}

-- if vim.fn.filereadable(vim.fn.expand("~/.vimrc_background")) then
--   vim.g.base16colorspace = 256
--   vim.cmd([[source ~/.vimrc_background]])
-- end

-- local colors = { black = "#202328", bg = "#202328" }
--
-- vim.cmd("hi StatusLine guibg=" .. colors.bg)
-- vim.cmd([[highlight SpecialKey ctermfg=19 guifg=#333333]])
-- vim.cmd([[highlight NonText ctermfg=19 guifg=#333333]])

-- vim.cmd([[highlight MatchParen guibg=none guifg=#555555]])

vim.g.ranger_map_keys = 0

vim.cmd([[runtime plugin/astronauta.vim]])

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1
vim.g.loaded_netrwSettings = 1
-- vim.g.loaded_matchit = 1
vim.g.loaded_gzip = 1
vim.g.loaded_tar = 1
vim.g.loaded_tarPlugin = 1
vim.g.loaded_tutor_mode_plugin = 1
vim.g.loaded_zip = 1
vim.g.loaded_zipPlugin = 1

vim.g.border_style = "edge"

vim.cmd("autocmd BufEnter * setlocal formatoptions-=cro")

local myColors = {
  bg = "#1a1b26",
}
-- require("kanagawa").setup({ colors = myColors })

vim.g.tokyonight_style = "night"
vim.g.gruvbox_contrast_dark = "hard"
vim.g.tokyonight_transparent_sidebar = true
vim.g.tokyonight_transparent = true
vim.g.gruvbox_invert_selection = "0"
vim.opt.background = "dark"
-- vim.cmd("colorscheme tokyonight")

vim.g.catppuccin_flavour = "mocha"
-- vim.cmd("colorscheme kanagawa")
-- require("gruvbox").setup({
--   overrides = {
--     SignColumn = { bg = "None" },
--     SignAdd = { bg = "None", fg = "#ffffff" },
--     SignChange = { bg = "None" },
--     SignDelete = { bg = "None" },
--     GitSignsAdd = { bg = "None" },
--     GitSignsChange = { bg = "None" },
--     GitSignsDelete = { bg = "None" },
--   },
-- })

vim.cmd("colorscheme kanagawa")

local hl = function(thing, opts)
  vim.api.nvim_set_hl(0, thing, opts)
end

hl("SignColumn", {
  bg = "none",
})

hl("ColorColumn", {
  ctermbg = 0,
  bg = "#555555",
})

hl("CursorLineNR", {
  bg = "None",
})

hl("Normal", {
  bg = "none",
})

hl("LineNr", {
  fg = "#5eacd3",
})

hl("netrwDir", {
  fg = "#5eacd3",
})

-- require("base16-colorscheme").setup({
--   base00 = "#000000",
--   base01 = "#3c3836",
--   base02 = "#504945",
--   base03 = "#665c54",
--   base04 = "#bdae93",
--   base05 = "#d5c4a1",
--   base06 = "#ebdbb2",
--   base07 = "#fbf1c7",
--   base08 = "#fb4934",
--   base09 = "#fe8019",
--   base0A = "#fabd2f",
--   base0B = "#b8bb26",
--   base0C = "#8ec07c",
--   base0D = "#83a598",
--   base0E = "#d3869b",
--   base0F = "#d65d0e",
-- })

vim.g.mapleader = " "
require("config")
require("plugins")
require("lsp")
