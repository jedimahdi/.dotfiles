local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
  execute 'packadd packer.nvim'
end

vim.cmd 'autocmd BufWritePost plugins.lua PackerCompile' -- Auto compile when there are changes in plugins.lua

return require('packer').startup(function(use)
  use 'wbthomason/packer.nvim'

  -- Theme
  use 'ulwlu/elly.vim'
  use { 'christianchiarulli/nvcode-color-schemes.vim' }
  use 'joshdick/onedark.vim'
  -- use 'navarasu/onedark.nvim'
  use 'glepnir/zephyr-nvim'
  use 'kyazdani42/nvim-web-devicons'
  use { 'ryanoasis/vim-devicons', opt = true }
  use 'glepnir/galaxyline.nvim'
  use 'folke/tokyonight.nvim'
  use 'bluz71/vim-moonfly-colors'
  use 'shaunsingh/moonlight.nvim'
  -- use 'RRethy/nvim-base16'

  -- File finder
  use 'nvim-lua/popup.nvim'
  use 'nvim-lua/plenary.nvim'
  use 'nvim-telescope/telescope.nvim'
  use 'nvim-telescope/telescope-fzy-native.nvim'

  -- File tree
  use 'kyazdani42/nvim-tree.lua'

  -- Comment
  use 'b3nj5m1n/kommentary'

  -- Syntax
  use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
  use { 'neovimhaskell/haskell-vim' }
  use 'norcalli/nvim-colorizer.lua'
  use 'purescript-contrib/purescript-vim'

  -- LSP
  use 'neovim/nvim-lspconfig'
  use 'glepnir/lspsaga.nvim'
  use { '~/apps/lua/nvim-lspinstall' }
  use 'folke/trouble.nvim'

  -- Autocomplete
  use 'hrsh7th/nvim-compe'
  use 'hrsh7th/vim-vsnip'

  -- Git
  use 'TimUntersberger/neogit'
  use { 'lewis6991/gitsigns.nvim', requires = { 'nvim-lua/plenary.nvim' } }

  -- Format
  use 'sbdchd/neoformat'

  -- Navigation
  use 'unblevable/quick-scope'

  -- Search
  use 'windwp/nvim-spectre'

  -- Other
  use 'tpope/vim-surround'
  -- use 'itchyny/vim-cursorword'
  use 'hrsh7th/vim-eft'
  use 'windwp/nvim-autopairs'
  use 'windwp/nvim-ts-autotag'
  use 'moll/vim-bbye'
  use 'rhysd/accelerated-jk'

  use 'folke/which-key.nvim'

  use 'tpope/vim-dadbod'
  use 'kristijanhusak/vim-dadbod-ui'
end)
