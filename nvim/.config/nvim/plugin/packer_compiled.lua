-- Automatically generated packer.nvim plugin loader code

if vim.api.nvim_call_function('has', {'nvim-0.5'}) ~= 1 then
  vim.api.nvim_command('echohl WarningMsg | echom "Invalid Neovim version for packer.nvim! | echohl None"')
  return
end

vim.api.nvim_command('packadd packer.nvim')

local no_errors, error_msg = pcall(function()

  local time
  local profile_info
  local should_profile = false
  if should_profile then
    local hrtime = vim.loop.hrtime
    profile_info = {}
    time = function(chunk, start)
      if start then
        profile_info[chunk] = hrtime()
      else
        profile_info[chunk] = (hrtime() - profile_info[chunk]) / 1e6
      end
    end
  else
    time = function(chunk, start) end
  end
  
local function save_profiles(threshold)
  local sorted_times = {}
  for chunk_name, time_taken in pairs(profile_info) do
    sorted_times[#sorted_times + 1] = {chunk_name, time_taken}
  end
  table.sort(sorted_times, function(a, b) return a[2] > b[2] end)
  local results = {}
  for i, elem in ipairs(sorted_times) do
    if not threshold or threshold and elem[2] > threshold then
      results[i] = elem[1] .. ' took ' .. elem[2] .. 'ms'
    end
  end

  _G._packer = _G._packer or {}
  _G._packer.profile_output = results
end

time([[Luarocks path setup]], true)
local package_path_str = "/home/mahdi/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?.lua;/home/mahdi/.cache/nvim/packer_hererocks/2.1.0-beta3/share/lua/5.1/?/init.lua;/home/mahdi/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?.lua;/home/mahdi/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/luarocks/rocks-5.1/?/init.lua"
local install_cpath_pattern = "/home/mahdi/.cache/nvim/packer_hererocks/2.1.0-beta3/lib/lua/5.1/?.so"
if not string.find(package.path, package_path_str, 1, true) then
  package.path = package.path .. ';' .. package_path_str
end

if not string.find(package.cpath, install_cpath_pattern, 1, true) then
  package.cpath = package.cpath .. ';' .. install_cpath_pattern
end

time([[Luarocks path setup]], false)
time([[try_loadstring definition]], true)
local function try_loadstring(s, component, name)
  local success, result = pcall(loadstring(s), name, _G.packer_plugins[name])
  if not success then
    vim.schedule(function()
      vim.api.nvim_notify('packer.nvim: Error running ' .. component .. ' for ' .. name .. ': ' .. result, vim.log.levels.ERROR, {})
    end)
  end
  return result
end

time([[try_loadstring definition]], false)
time([[Defining packer_plugins]], true)
_G.packer_plugins = {
  ["Comment.nvim"] = {
    config = { "require('plugins.comment')" },
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/Comment.nvim",
    url = "https://github.com/numToStr/Comment.nvim"
  },
  LuaSnip = {
    config = { "require('plugins.luasnip')" },
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/LuaSnip",
    url = "https://github.com/L3MON4D3/LuaSnip"
  },
  ["bclose.vim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/bclose.vim",
    url = "https://github.com/rbgrouleff/bclose.vim"
  },
  ["cmp-buffer"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/cmp-buffer",
    url = "https://github.com/hrsh7th/cmp-buffer"
  },
  ["cmp-nvim-lsp"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/cmp-nvim-lsp",
    url = "https://github.com/hrsh7th/cmp-nvim-lsp"
  },
  ["cmp-nvim-lua"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/cmp-nvim-lua",
    url = "https://github.com/hrsh7th/cmp-nvim-lua"
  },
  ["cmp-path"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/cmp-path",
    url = "https://github.com/hrsh7th/cmp-path"
  },
  cmp_luasnip = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/cmp_luasnip",
    url = "https://github.com/saadparwaiz1/cmp_luasnip"
  },
  ["dhall-vim"] = {
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/dhall-vim",
    url = "https://github.com/vmchale/dhall-vim"
  },
  ["fidget.nvim"] = {
    config = { "\27LJ\2\n8\0\0\3\0\3\0\a6\0\0\0'\2\1\0B\0\2\0029\0\2\0004\2\0\0B\0\2\1K\0\1\0\nsetup\vfidget\frequire\0" },
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/fidget.nvim",
    url = "https://github.com/j-hui/fidget.nvim"
  },
  ["gitsigns.nvim"] = {
    config = { "require('plugins.git')" },
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/gitsigns.nvim",
    url = "https://github.com/lewis6991/gitsigns.nvim"
  },
  harpoon = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/harpoon",
    url = "https://github.com/ThePrimeagen/harpoon"
  },
  ["haskell-vim"] = {
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/haskell-vim",
    url = "https://github.com/neovimhaskell/haskell-vim"
  },
  ["impatient.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/impatient.nvim",
    url = "https://github.com/lewis6991/impatient.nvim"
  },
  ["lightspeed.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/lightspeed.nvim",
    url = "https://github.com/ggandor/lightspeed.nvim"
  },
  ["lir-git-status.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/lir-git-status.nvim",
    url = "https://github.com/tamago324/lir-git-status.nvim"
  },
  ["lir-mmv.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/lir-mmv.nvim",
    url = "https://github.com/tamago324/lir-mmv.nvim"
  },
  ["lir.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/lir.nvim",
    url = "https://github.com/tamago324/lir.nvim"
  },
  ["lspkind-nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/lspkind-nvim",
    url = "https://github.com/onsails/lspkind-nvim"
  },
  ["markdown-preview.nvim"] = {
    commands = { "MarkdownPreview" },
    config = { "vim.cmd[[doautocmd BufEnter]]" },
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/markdown-preview.nvim",
    url = "https://github.com/iamcco/markdown-preview.nvim"
  },
  ["null-ls.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/null-ls.nvim",
    url = "https://github.com/jose-elias-alvarez/null-ls.nvim"
  },
  ["nvim-autopairs"] = {
    config = { "require('plugins.autopairs')" },
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-autopairs",
    url = "https://github.com/windwp/nvim-autopairs",
    wants = { "nvim-cmp" }
  },
  ["nvim-base16"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-base16",
    url = "https://github.com/RRethy/nvim-base16"
  },
  ["nvim-cmp"] = {
    config = { "require('plugins.cmp')" },
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-cmp",
    url = "https://github.com/hrsh7th/nvim-cmp"
  },
  ["nvim-lsp-ts-utils"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-lsp-ts-utils",
    url = "https://github.com/jose-elias-alvarez/nvim-lsp-ts-utils"
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-lspconfig",
    url = "https://github.com/neovim/nvim-lspconfig"
  },
  ["nvim-lspinstall"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-lspinstall",
    url = "https://github.com/kabouzeid/nvim-lspinstall"
  },
  ["nvim-treesitter"] = {
    config = { "\27LJ\2\n@\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\23plugins.treesitter\frequire\0" },
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-treesitter",
    url = "https://github.com/nvim-treesitter/nvim-treesitter"
  },
  ["nvim-ts-autotag"] = {
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/nvim-ts-autotag",
    url = "https://github.com/windwp/nvim-ts-autotag"
  },
  ["nvim-ts-context-commentstring"] = {
    loaded = false,
    needs_bufread = false,
    only_cond = false,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/nvim-ts-context-commentstring",
    url = "https://github.com/JoosepAlviste/nvim-ts-context-commentstring"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-web-devicons",
    url = "https://github.com/kyazdani42/nvim-web-devicons"
  },
  ["onedarker.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/onedarker.nvim",
    url = "https://github.com/LunarVim/onedarker.nvim"
  },
  onehalf = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/onehalf/vim",
    url = "https://github.com/sonph/onehalf"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/packer.nvim",
    url = "https://github.com/wbthomason/packer.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/plenary.nvim",
    url = "https://github.com/nvim-lua/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/popup.nvim",
    url = "https://github.com/nvim-lua/popup.nvim"
  },
  ["purescript-vim"] = {
    loaded = false,
    needs_bufread = true,
    only_cond = false,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/purescript-vim",
    url = "https://github.com/purescript-contrib/purescript-vim"
  },
  ["ranger.vim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/ranger.vim",
    url = "https://github.com/francoiscabrol/ranger.vim"
  },
  tabular = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/tabular",
    url = "https://github.com/godlygeek/tabular"
  },
  ["telescope-fzf-native.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/telescope-fzf-native.nvim",
    url = "https://github.com/nvim-telescope/telescope-fzf-native.nvim"
  },
  ["telescope.nvim"] = {
    config = { "require('plugins.telescope')" },
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/telescope.nvim",
    url = "https://github.com/nvim-telescope/telescope.nvim"
  },
  ["vim-fugitive"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/vim-fugitive",
    url = "https://github.com/tpope/vim-fugitive"
  },
  ["vim-repeat"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/vim-repeat",
    url = "https://github.com/tpope/vim-repeat"
  },
  ["vim-surround"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/vim-surround",
    url = "https://github.com/tpope/vim-surround"
  }
}

time([[Defining packer_plugins]], false)
-- Runtimepath customization
time([[Runtimepath customization]], true)
vim.o.runtimepath = vim.o.runtimepath .. ",/home/mahdi/.local/share/nvim/site/pack/packer/start/onehalf/vim"
time([[Runtimepath customization]], false)
-- Config for: telescope.nvim
time([[Config for telescope.nvim]], true)
require('plugins.telescope')
time([[Config for telescope.nvim]], false)
-- Config for: LuaSnip
time([[Config for LuaSnip]], true)
require('plugins.luasnip')
time([[Config for LuaSnip]], false)
-- Config for: nvim-autopairs
time([[Config for nvim-autopairs]], true)
require('plugins.autopairs')
time([[Config for nvim-autopairs]], false)
-- Config for: Comment.nvim
time([[Config for Comment.nvim]], true)
require('plugins.comment')
time([[Config for Comment.nvim]], false)
-- Config for: nvim-treesitter
time([[Config for nvim-treesitter]], true)
try_loadstring("\27LJ\2\n@\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\23plugins.treesitter\frequire\0", "config", "nvim-treesitter")
time([[Config for nvim-treesitter]], false)
-- Config for: nvim-cmp
time([[Config for nvim-cmp]], true)
require('plugins.cmp')
time([[Config for nvim-cmp]], false)
-- Config for: fidget.nvim
time([[Config for fidget.nvim]], true)
try_loadstring("\27LJ\2\n8\0\0\3\0\3\0\a6\0\0\0'\2\1\0B\0\2\0029\0\2\0004\2\0\0B\0\2\1K\0\1\0\nsetup\vfidget\frequire\0", "config", "fidget.nvim")
time([[Config for fidget.nvim]], false)
-- Config for: gitsigns.nvim
time([[Config for gitsigns.nvim]], true)
require('plugins.git')
time([[Config for gitsigns.nvim]], false)

-- Command lazy-loads
time([[Defining lazy-load commands]], true)
pcall(vim.cmd, [[command -nargs=* -range -bang -complete=file MarkdownPreview lua require("packer.load")({'markdown-preview.nvim'}, { cmd = "MarkdownPreview", l1 = <line1>, l2 = <line2>, bang = <q-bang>, args = <q-args>, mods = "<mods>" }, _G.packer_plugins)]])
time([[Defining lazy-load commands]], false)

vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
time([[Defining lazy-load filetype autocommands]], true)
vim.cmd [[au FileType javascript ++once lua require("packer.load")({'nvim-ts-autotag', 'nvim-ts-context-commentstring'}, { ft = "javascript" }, _G.packer_plugins)]]
vim.cmd [[au FileType purescript ++once lua require("packer.load")({'purescript-vim'}, { ft = "purescript" }, _G.packer_plugins)]]
vim.cmd [[au FileType purs ++once lua require("packer.load")({'purescript-vim'}, { ft = "purs" }, _G.packer_plugins)]]
vim.cmd [[au FileType typescript ++once lua require("packer.load")({'nvim-ts-autotag', 'nvim-ts-context-commentstring'}, { ft = "typescript" }, _G.packer_plugins)]]
vim.cmd [[au FileType typescriptreact ++once lua require("packer.load")({'nvim-ts-autotag', 'nvim-ts-context-commentstring'}, { ft = "typescriptreact" }, _G.packer_plugins)]]
vim.cmd [[au FileType dhall ++once lua require("packer.load")({'dhall-vim'}, { ft = "dhall" }, _G.packer_plugins)]]
vim.cmd [[au FileType markdown ++once lua require("packer.load")({'markdown-preview.nvim'}, { ft = "markdown" }, _G.packer_plugins)]]
vim.cmd [[au FileType haskell ++once lua require("packer.load")({'haskell-vim'}, { ft = "haskell" }, _G.packer_plugins)]]
vim.cmd [[au FileType javascriptreact ++once lua require("packer.load")({'nvim-ts-autotag', 'nvim-ts-context-commentstring'}, { ft = "javascriptreact" }, _G.packer_plugins)]]
time([[Defining lazy-load filetype autocommands]], false)
vim.cmd("augroup END")
vim.cmd [[augroup filetypedetect]]
time([[Sourcing ftdetect script at: /home/mahdi/.local/share/nvim/site/pack/packer/opt/purescript-vim/ftdetect/purescript.vim]], true)
vim.cmd [[source /home/mahdi/.local/share/nvim/site/pack/packer/opt/purescript-vim/ftdetect/purescript.vim]]
time([[Sourcing ftdetect script at: /home/mahdi/.local/share/nvim/site/pack/packer/opt/purescript-vim/ftdetect/purescript.vim]], false)
time([[Sourcing ftdetect script at: /home/mahdi/.local/share/nvim/site/pack/packer/opt/haskell-vim/ftdetect/haskell.vim]], true)
vim.cmd [[source /home/mahdi/.local/share/nvim/site/pack/packer/opt/haskell-vim/ftdetect/haskell.vim]]
time([[Sourcing ftdetect script at: /home/mahdi/.local/share/nvim/site/pack/packer/opt/haskell-vim/ftdetect/haskell.vim]], false)
time([[Sourcing ftdetect script at: /home/mahdi/.local/share/nvim/site/pack/packer/opt/dhall-vim/ftdetect/dhall.vim]], true)
vim.cmd [[source /home/mahdi/.local/share/nvim/site/pack/packer/opt/dhall-vim/ftdetect/dhall.vim]]
time([[Sourcing ftdetect script at: /home/mahdi/.local/share/nvim/site/pack/packer/opt/dhall-vim/ftdetect/dhall.vim]], false)
vim.cmd("augroup END")
if should_profile then save_profiles() end

end)

if not no_errors then
  error_msg = error_msg:gsub('"', '\\"')
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
