_ = vim.cmd([[packadd packer.nvim]])
_ = vim.cmd([[packadd vimball]])

return require("packer").startup({
  function(use)
    local config = function(name)
      return string.format("require('plugins.%s')", name)
    end

    local use_with_config = function(path, name)
      use({ path, config = config(name) })
    end

    use("wbthomason/packer.nvim")
    use("lewis6991/impatient.nvim")
    use_with_config("~/tmp/filetype.nvim", "filetype") -- greatly reduces startup time

    use("nvim-lua/popup.nvim")
    use("nvim-lua/plenary.nvim")
    use("neovim/nvim-lspconfig")
    use({
      "j-hui/fidget.nvim",
      config = function()
        require("fidget").setup({})
      end,
    })

    -- use("rebelot/kanagawa.nvim")
    -- use("glepnir/zephyr-nvim")
    use("LunarVim/onedarker.nvim")
    use("kyazdani42/nvim-web-devicons")
    -- use("folke/tokyonight.nvim")
    use("RRethy/nvim-base16")
    -- use_with_config("nvim-lualine/lualine.nvim", "lualine")

    use("jose-elias-alvarez/null-ls.nvim")
    use("jose-elias-alvarez/nvim-lsp-ts-utils")
    use("kabouzeid/nvim-lspinstall")
    use("onsails/lspkind-nvim")

    -- Comment
    use_with_config("numToStr/Comment.nvim", "comment")

    -- Syntax
    use({
      "nvim-treesitter/nvim-treesitter",
      run = ":TSUpdate",
      config = function()
        require("plugins.treesitter").setup()
      end,
    })
    use({ "windwp/nvim-ts-autotag", ft = { "typescript", "typescriptreact", "javascript", "javascriptreact" } })
    use({
      "JoosepAlviste/nvim-ts-context-commentstring",
      ft = { "typescript", "typescriptreact", "javascript", "javascriptreact" },
    }) -- makes jsx comments actually work
    use({ "neovimhaskell/haskell-vim", ft = "haskell" })
    -- use { 'LnL7/vim-nix', ft = 'nix' }
    use({ "vmchale/dhall-vim", ft = "dhall" })
    use({ "purescript-contrib/purescript-vim", ft = { "purescript", "purs" } })
    -- use({ "mlochbaum/BQN", ft = "bqn", rtp = "editors/vim" })
    -- use { 'norcalli/nvim-colorizer.lua' }

    -- use({ "rafamadriz/friendly-snippets" })

    use_with_config("L3MON4D3/LuaSnip", "luasnip")

    use({
      "hrsh7th/nvim-cmp",
      requires = {
        "hrsh7th/cmp-nvim-lsp",
        "hrsh7th/cmp-nvim-lua",
        "hrsh7th/cmp-buffer",
        -- "hrsh7th/cmp-vsnip",
        "saadparwaiz1/cmp_luasnip",
        "hrsh7th/cmp-path",
      },
      config = config("cmp"),
    })

    -- Git
    use_with_config("lewis6991/gitsigns.nvim", "git")

    use({
      "windwp/nvim-autopairs",
      wants = "nvim-cmp",
      config = config("autopairs"),
    })

    use("tpope/vim-surround")
    use("tpope/vim-repeat")
    use("tpope/vim-fugitive")
    use("godlygeek/tabular")

    use("ggandor/lightspeed.nvim") -- motion
    -- use_with_config("svermeulen/vim-cutlass", "cutlass") -- separates cut and delete operations
    -- use_with_config("svermeulen/vim-yoink", "yoink") -- improves paste
    use_with_config("ibhagwan/fzf-lua", "fzf")
    use("rbgrouleff/bclose.vim")
    use("francoiscabrol/ranger.vim")
    use("tamago324/lir.nvim")
    use("tamago324/lir-git-status.nvim")
    use("tamago324/lir-mmv.nvim")
    use({
      "iamcco/markdown-preview.nvim", -- preview markdown output in browser
      opt = true,
      ft = { "markdown" },
      config = "vim.cmd[[doautocmd BufEnter]]",
      run = "cd app && yarn install",
      cmd = "MarkdownPreview",
    })
    use("andymass/vim-matchup")
    use("ray-x/lsp_signature.nvim")
  end,
  config = {
    git = { clone_timeout = 300 },
    display = {
      open_fn = function()
        return require("packer.util").float({ border = "single" })
      end,
    },
  },
})
