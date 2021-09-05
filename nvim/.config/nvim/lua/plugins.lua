local execute = vim.api.nvim_command
local fn = vim.fn

local install_path = DATA_PATH .. '/site/pack/packer/start/packer.nvim'

if fn.empty(fn.glob(install_path)) > 0 then
  fn.system({'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path})
  vim.cmd 'packadd packer.nvim'
end

-- if fn.empty(fn.glob(install_path)) > 0 then
  -- execute('!git clone https://github.com/wbthomason/packer.nvim ' .. install_path)
  -- execute 'packadd packer.nvim'
-- end

vim.cmd 'autocmd BufWritePost plugins.lua PackerCompile'

local packer_ok, packer = pcall(require, 'packer')
if not packer_ok then return end

return packer.startup {
  function(use)
    use 'wbthomason/packer.nvim'

       -- LSP
    use 'neovim/nvim-lspconfig'
    use { 'kabouzeid/nvim-lspinstall', event = 'VimEnter' }
    use 'glepnir/lspsaga.nvim'
    use { 'tjdevries/astronauta.nvim' }
    -- use 'folke/trouble.nvim'

    -- Theme
    use 'glepnir/zephyr-nvim'
    use 'kyazdani42/nvim-web-devicons'
    use { 'ryanoasis/vim-devicons', opt = true }
    use 'glepnir/galaxyline.nvim'
    -- use 'joshdick/onedark.vim'

    -- File finder
    use { 'nvim-lua/popup.nvim' }
    use { 'nvim-lua/plenary.nvim' }
    use { 'nvim-telescope/telescope.nvim' }

    -- File tree
    use {
      'kyazdani42/nvim-tree.lua',
      config = function()
        require 'plugins.nvimtree'
      end,
    }

    -- Comment
    use {
      'terrortylor/nvim-comment',
      event = 'BufRead',
      config = function()
        local status_ok, nvim_comment = pcall(require, 'nvim_comment')
        if not status_ok then return end
        nvim_comment.setup()
      end,
    }

    -- Syntax
    use { 'nvim-treesitter/nvim-treesitter', run = ':TSUpdate' }
    use { 'neovimhaskell/haskell-vim', ft = 'haskell' }
    use { 'LnL7/vim-nix', ft = 'nix' }
    use { 'vmchale/dhall-vim', ft = 'dhall' }
    use { 'purescript-contrib/purescript-vim', ft = 'purescript' }
    -- use { 'norcalli/nvim-colorizer.lua' }
    -- use { 'p00f/nvim-ts-rainbow' }

    -- Autocomplete
    use {
      'hrsh7th/nvim-compe',
      -- event = 'InsertEnter',
      config = function()
        require 'plugins.compe'
      end,
    }
    use {
      'hrsh7th/vim-vsnip',
      -- event = 'InsertEnter'
    }

    -- Git
    use {
      'lewis6991/gitsigns.nvim',
      requires = { 'nvim-lua/plenary.nvim' },
      config = function()
        require 'plugins.gitsigns'
      end,
      event = 'BufRead',
    }

    -- Search
    -- use { 'windwp/nvim-spectre' }

    -- Other
    -- use 'tpope/vim-surround'
    use {
      'steelsojka/pears.nvim',
      -- event = 'InsertEnter',
      config = function()
        require('pears').setup()
      end,
    }

    use { 'folke/which-key.nvim' }

    -- use { 'editorconfig/editorconfig-vim' }
    -- use {
    --   'mhartington/formatter.nvim',
    --   config = function()
    --     require 'plugins.formatter'
    --   end,
    --   event = 'BufRead',
    -- }

    use 'tpope/vim-surround'

    -- LSP
  end,
  config = {
    git = { clone_timeout = 300 },
    display = {
      open_fn = function()
        return require('packer.util').float { border = 'single' }
      end,
    },
  },
}
