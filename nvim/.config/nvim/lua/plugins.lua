local execute = vim.api.nvim_command
local fn = vim.fn
local packer = require('packer')

local install_path = fn.stdpath('data') .. '/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
  execute 'packadd packer.nvim'
end

vim.cmd 'autocmd BufWritePost plugins.lua PackerCompile' -- Auto compile when there are changes in plugins.lua

return packer.startup({
  function(use)
    use 'wbthomason/packer.nvim'

    -- LSP
    use 'neovim/nvim-lspconfig'
    use { 'kabouzeid/nvim-lspinstall', event = 'VimEnter' }
    use 'glepnir/lspsaga.nvim'
    use 'folke/trouble.nvim'

    -- Theme
    use 'christianchiarulli/nvcode-color-schemes.vim'
    use 'glepnir/zephyr-nvim'
    use 'kyazdani42/nvim-web-devicons'
    use { 'ryanoasis/vim-devicons', opt = true }
    use 'glepnir/galaxyline.nvim'
    -- use 'folke/tokyonight.nvim'
    -- use 'ghifarit53/tokyonight-vim'
    -- use 'bluz71/vim-moonfly-colors'
    -- use 'w0ng/vim-hybrid'
    -- use 'sainnhe/everforest'
    -- use 'rakr/vim-one'
    -- use 'sonph/onehalf'
    -- use 'sheerun/vim-polyglot'
    -- use 'shaunsingh/moonlight.nvim'
    -- use 'RRethy/nvim-base16'

    -- File finder
    use 'nvim-lua/popup.nvim'
    use 'nvim-lua/plenary.nvim'
    use 'nvim-telescope/telescope.nvim'
    use 'nvim-telescope/telescope-fzy-native.nvim'

    -- File tree
    use { 'kyazdani42/nvim-tree.lua' }

    -- Comment
    use { 'b3nj5m1n/kommentary' }

    -- Syntax
    use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
    use { 'neovimhaskell/haskell-vim', ft = 'haskell' }
    use { 'norcalli/nvim-colorizer.lua' }
    use { 'purescript-contrib/purescript-vim' }
    use { 'p00f/nvim-ts-rainbow' }

    -- Autocomplete
    use {
      'hrsh7th/nvim-compe',
      event = 'InsertEnter',
      config = function()
        require('plugins.compe')
      end,
    }
    use { 'hrsh7th/vim-vsnip', event = 'InsertEnter' }

    -- Git
    use { 'TimUntersberger/neogit' }
    use {
      'lewis6991/gitsigns.nvim',
      requires = { 'nvim-lua/plenary.nvim' },
      config = function()
        require('plugins.gitsigns').config()
      end,
      event = 'BufRead',
    }

    -- Search
    use { 'windwp/nvim-spectre' }

    -- Other
    use 'tpope/vim-surround'
    -- use 'itchyny/vim-cursorword'
    use 'hrsh7th/vim-eft'
    use {
      'steelsojka/pears.nvim',
      event = 'InsertEnter',
      config = function()
        require('pears').config()
      end,
    }
    -- use 'windwp/nvim-autopairs'
    -- use 'windwp/nvim-ts-autotag'
    -- use 'moll/vim-bbye'
    -- use 'rhysd/accelerated-jk'

    use { 'folke/which-key.nvim' }

    --[[ use 'tpope/vim-dadbod'
  use 'kristijanhusak/vim-dadbod-ui' ]]
  end,
  config = {
    git = { clone_timeout = 300 },
    display = {
      open_fn = function()
        return require('packer.util').float({ border = 'single' })
      end,
    },
  },
})
