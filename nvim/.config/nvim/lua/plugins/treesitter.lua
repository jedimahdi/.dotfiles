require('nvim-treesitter.configs').setup {
  -- ensure_installed = {
  --   'bash', 'javascript', 'json', 'lua', 'typescript', 'tsx', 'html', 'css', 'c', 'cpp', 'toml', 'scss', 'yaml', 'nix',
  --   'latex', 'elm', 'dockerfile', 'clojure', 'cmake'
  -- },
  ensure_installed = 'maintained',
  highlight = { enable = true, disable = { 'haskell' } },
  indent = { enable = false },
  autotag = { enable = true },
}
