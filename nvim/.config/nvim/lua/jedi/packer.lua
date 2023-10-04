return require("packer").startup(function(use)
  use("wbthomason/packer.nvim")
  use("nvim-lua/plenary.nvim")
  use("nvim-lua/popup.nvim")
  use("kyazdani42/nvim-web-devicons")

  -- Format
  use("sbdchd/neoformat")

  -- Telescope and Navigation
  use("nvim-telescope/telescope.nvim")
  use({ "nvim-telescope/telescope-fzf-native.nvim", run = "make" })
  use("ThePrimeagen/harpoon")
  use("tamago324/lir.nvim")

  -- LSP
  use("neovim/nvim-lspconfig")
  use("onsails/lspkind-nvim")
  -- use("glepnir/lspsaga.nvim")
  use("nvim-lua/lsp_extensions.nvim")
  use("simrat39/symbols-outline.nvim")
  -- use("j-hui/fidget.nvim")
  use("folke/neodev.nvim")
  use("simrat39/rust-tools.nvim")
  use({
    "ray-x/lsp_signature.nvim",
  })
  -- use("b0o/schemastore.nvim")

  -- cmp
  use("hrsh7th/cmp-nvim-lsp")
  -- use("hrsh7th/cmp-path")
  use("hrsh7th/cmp-buffer")
  use("hrsh7th/nvim-cmp")
  use("dcampos/cmp-snippy")

  -- Snippet
  use("dcampos/nvim-snippy")

  -- Git
  -- use("ThePrimeagen/git-worktree.nvim")

  -- Color Scheme
  use("lunarvim/onedarker.nvim")
  use("navarasu/onedark.nvim")
  use("RRethy/nvim-base16")
  use({ "catppuccin/nvim", as = "catppuccin" })

  -- Tree sitter
  use("nvim-treesitter/nvim-treesitter", {
    run = ":TSUpdate",
  })
  use({
    "nvim-treesitter/nvim-treesitter-textobjects",
    after = "nvim-treesitter",
  })
  use("nvim-treesitter/playground")

  -- Comment
  use("numToStr/Comment.nvim")

  -- Auto Pairs
  use("windwp/nvim-autopairs")
  use("windwp/nvim-ts-autotag")

  -- Debug
  use("mfussenegger/nvim-dap")
  use("rcarriga/nvim-dap-ui")
  use("theHamsta/nvim-dap-virtual-text")

  -- Other
  use("mbbill/undotree")
  use("tpope/vim-surround")
  use("romainl/vim-cool")
  -- use("~/code/jedi")
  use("blazkowolf/gruber-darker.nvim")

  use({ "purescript-contrib/purescript-vim" })
  use({
    "mrcjkb/haskell-tools.nvim",
    branch = "2.x.x",
  })
  use({ "tjdevries/ocaml.nvim" })
  use({
    "pmizio/typescript-tools.nvim",
    requires = { "nvim-lua/plenary.nvim", "neovim/nvim-lspconfig" },
    -- config = function()
    --   require("typescript-tools").setup({})
    -- end,
  })

  use({
    "iamcco/markdown-preview.nvim", -- preview markdown output in browser
    -- opt = true,
    -- ft = { "markdown" },
    -- config = "vim.cmd[[doautocmd BufEnter]]",
    run = "cd app && yarn install",
    -- cmd = "MarkdownPreview",
  })
end)
