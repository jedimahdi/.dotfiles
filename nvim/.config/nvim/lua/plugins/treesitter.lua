require('nvim-treesitter.configs').setup {
  ensure_installed = { 'bash', 'javascript', 'json', 'lua', 'typescript', 'html', 'css', 'cpp' },
  highlight = { enable = true },
  indent = { enable = false },
  autotag = { enable = true },
}
