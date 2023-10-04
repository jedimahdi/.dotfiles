vim.g.gruvbox_contrast_dark = "hard"
vim.g.tokyonight_transparent_sidebar = true
vim.g.tokyonight_transparent = true
vim.g.gruvbox_invert_selection = "0"
vim.opt.background = "dark"


require('onedark').setup {
    style = 'darker'
}
require('onedark').load()

-- vim.cmd("colorscheme onedarker")

-- require("rose-pine").setup({
--   disable_background = true,
-- })
--
-- vim.cmd("colorscheme rose-pine")
--
-- vim.api.nvim_set_hl(0, "Normal", { bg = "none" })
-- vim.api.nvim_set_hl(0, "NormalFloat", { bg = "none" })
