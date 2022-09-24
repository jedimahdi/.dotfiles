return require("packer").startup(function(use)
  use("wbthomason/packer.nvim")
  use("sbdchd/neoformat")

  -- Simple plugins can be specified as strings
  use("TimUntersberger/neogit")

  -- TJ created lodash of neovim
  use("nvim-lua/plenary.nvim")
  use("nvim-lua/popup.nvim")
  use("nvim-telescope/telescope.nvim")

  -- All the things
  use("neovim/nvim-lspconfig")
  use("hrsh7th/cmp-nvim-lsp")
  use("hrsh7th/cmp-buffer")
  use("hrsh7th/nvim-cmp")
  use("onsails/lspkind-nvim")
  use("nvim-lua/lsp_extensions.nvim")
  use("glepnir/lspsaga.nvim")
  use("simrat39/symbols-outline.nvim")
  use("dcampos/nvim-snippy")
  use("dcampos/cmp-snippy")

  -- Primeagen doesn"t create lodash
  use("ThePrimeagen/git-worktree.nvim")
  use("ThePrimeagen/harpoon")

  use("mbbill/undotree")
  use("tamago324/lir.nvim")
  use("kyazdani42/nvim-web-devicons")

  -- Colorscheme section
  use("gruvbox-community/gruvbox")
  use("folke/tokyonight.nvim")
  use("lunarvim/onedarker.nvim")

  use("nvim-treesitter/nvim-treesitter", {
    run = ":TSUpdate",
  })

  use("nvim-treesitter/playground")

  use("mfussenegger/nvim-dap")
  use("rcarriga/nvim-dap-ui")
  use("theHamsta/nvim-dap-virtual-text")

  use("numToStr/Comment.nvim")
  use("windwp/nvim-autopairs")
  use("tpope/vim-surround")

  use({ "purescript-contrib/purescript-vim" })

  -- use({
  --   "iamcco/markdown-preview.nvim", -- preview markdown output in browser
  --   opt = true,
  --   ft = { "markdown" },
  --   config = "vim.cmd[[doautocmd BufEnter]]",
  --   run = "cd app && yarn install",
  --   cmd = "MarkdownPreview",
  -- })
end)
