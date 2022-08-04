vim.g.theprimeagen_colorscheme = "onedarker"

function ColorMyPencils()
  vim.g.gruvbox_contrast_dark = "hard"
  vim.g.tokyonight_transparent_sidebar = true
  vim.g.tokyonight_transparent = true
  vim.g.gruvbox_invert_selection = "0"
  vim.opt.background = "dark"

  vim.cmd("colorscheme " .. vim.g.theprimeagen_colorscheme)
end
ColorMyPencils()
