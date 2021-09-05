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
  local success, result = pcall(loadstring(s))
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
  ["astronauta.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/astronauta.nvim"
  },
  ["dhall-vim"] = {
    loaded = false,
    needs_bufread = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/dhall-vim"
  },
  ["galaxyline.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/galaxyline.nvim"
  },
  ["gitsigns.nvim"] = {
    config = { "\27LJ\2\n0\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\21plugins.gitsigns\frequire\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/gitsigns.nvim"
  },
  ["haskell-vim"] = {
    loaded = false,
    needs_bufread = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/haskell-vim"
  },
  ["lspsaga.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/lspsaga.nvim"
  },
  ["nvim-comment"] = {
    config = { "\27LJ\2\nP\0\0\4\0\4\0\n6\0\0\0006\2\1\0'\3\2\0B\0\3\3\14\0\0\0X\2\1€K\0\1\0009\2\3\1B\2\1\1K\0\1\0\nsetup\17nvim_comment\frequire\npcall\0" },
    loaded = false,
    needs_bufread = false,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/nvim-comment"
  },
  ["nvim-compe"] = {
    config = { "\27LJ\2\n-\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\18plugins.compe\frequire\0" },
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-compe"
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-lspconfig"
  },
  ["nvim-lspinstall"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/nvim-lspinstall"
  },
  ["nvim-tree.lua"] = {
    config = { "\27LJ\2\n0\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\21plugins.nvimtree\frequire\0" },
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/packer.nvim"
  },
  ["pears.nvim"] = {
    config = { "\27LJ\2\n3\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\npears\frequire\0" },
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/pears.nvim"
  },
  ["plenary.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/plenary.nvim"
  },
  ["popup.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/popup.nvim"
  },
  ["purescript-vim"] = {
    loaded = false,
    needs_bufread = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/purescript-vim"
  },
  ["telescope.nvim"] = {
    config = { "\27LJ\2\n1\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\22plugins.telescope\frequire\0" },
    loaded = false,
    needs_bufread = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/telescope.nvim"
  },
  ["vim-devicons"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/vim-devicons"
  },
  ["vim-nix"] = {
    loaded = false,
    needs_bufread = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/vim-nix"
  },
  ["vim-surround"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/vim-surround"
  },
  ["vim-vsnip"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/vim-vsnip"
  },
  ["which-key.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/which-key.nvim"
  },
  ["zephyr-nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/zephyr-nvim"
  }
}

time([[Defining packer_plugins]], false)
-- Config for: pears.nvim
time([[Config for pears.nvim]], true)
try_loadstring("\27LJ\2\n3\0\0\3\0\3\0\0066\0\0\0'\2\1\0B\0\2\0029\0\2\0B\0\1\1K\0\1\0\nsetup\npears\frequire\0", "config", "pears.nvim")
time([[Config for pears.nvim]], false)
-- Config for: nvim-compe
time([[Config for nvim-compe]], true)
try_loadstring("\27LJ\2\n-\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\18plugins.compe\frequire\0", "config", "nvim-compe")
time([[Config for nvim-compe]], false)
-- Config for: nvim-tree.lua
time([[Config for nvim-tree.lua]], true)
try_loadstring("\27LJ\2\n0\0\0\3\0\2\0\0046\0\0\0'\2\1\0B\0\2\1K\0\1\0\21plugins.nvimtree\frequire\0", "config", "nvim-tree.lua")
time([[Config for nvim-tree.lua]], false)
vim.cmd [[augroup packer_load_aucmds]]
vim.cmd [[au!]]
  -- Filetype lazy-loads
time([[Defining lazy-load filetype autocommands]], true)
vim.cmd [[au FileType haskell ++once lua require("packer.load")({'haskell-vim'}, { ft = "haskell" }, _G.packer_plugins)]]
vim.cmd [[au FileType dhall ++once lua require("packer.load")({'dhall-vim'}, { ft = "dhall" }, _G.packer_plugins)]]
vim.cmd [[au FileType purescript ++once lua require("packer.load")({'purescript-vim'}, { ft = "purescript" }, _G.packer_plugins)]]
vim.cmd [[au FileType nix ++once lua require("packer.load")({'vim-nix'}, { ft = "nix" }, _G.packer_plugins)]]
time([[Defining lazy-load filetype autocommands]], false)
  -- Event lazy-loads
time([[Defining lazy-load event autocommands]], true)
vim.cmd [[au VimEnter * ++once lua require("packer.load")({'nvim-lspinstall'}, { event = "VimEnter *" }, _G.packer_plugins)]]
vim.cmd [[au BufWinEnter * ++once lua require("packer.load")({'telescope.nvim'}, { event = "BufWinEnter *" }, _G.packer_plugins)]]
vim.cmd [[au BufRead * ++once lua require("packer.load")({'gitsigns.nvim', 'nvim-comment'}, { event = "BufRead *" }, _G.packer_plugins)]]
time([[Defining lazy-load event autocommands]], false)
vim.cmd("augroup END")
vim.cmd [[augroup filetypedetect]]
time([[Sourcing ftdetect script at: /home/mahdi/.local/share/nvim/site/pack/packer/opt/purescript-vim/ftdetect/purescript.vim]], true)
vim.cmd [[source /home/mahdi/.local/share/nvim/site/pack/packer/opt/purescript-vim/ftdetect/purescript.vim]]
time([[Sourcing ftdetect script at: /home/mahdi/.local/share/nvim/site/pack/packer/opt/purescript-vim/ftdetect/purescript.vim]], false)
time([[Sourcing ftdetect script at: /home/mahdi/.local/share/nvim/site/pack/packer/opt/vim-nix/ftdetect/nix.vim]], true)
vim.cmd [[source /home/mahdi/.local/share/nvim/site/pack/packer/opt/vim-nix/ftdetect/nix.vim]]
time([[Sourcing ftdetect script at: /home/mahdi/.local/share/nvim/site/pack/packer/opt/vim-nix/ftdetect/nix.vim]], false)
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
  vim.api.nvim_command('echohl ErrorMsg | echom "Error in packer_compiled: '..error_msg..'" | echom "Please check your config for correctness" | echohl None')
end
