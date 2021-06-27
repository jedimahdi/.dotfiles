" Automatically generated packer.nvim plugin loader code

if !has('nvim-0.5')
  echohl WarningMsg
  echom "Invalid Neovim version for packer.nvim!"
  echohl None
  finish
endif

packadd packer.nvim

try

lua << END
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
  ["accelerated-jk"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/accelerated-jk"
  },
  ["elly.vim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/elly.vim"
  },
  ["galaxyline.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/galaxyline.nvim"
  },
  ["gitsigns.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/gitsigns.nvim"
  },
  ["haskell-vim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/haskell-vim"
  },
  kommentary = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/kommentary"
  },
  ["lspsaga.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/lspsaga.nvim"
  },
  ["moonlight.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/moonlight.nvim"
  },
  neoformat = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/neoformat"
  },
  neogit = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/neogit"
  },
  ["nvcode-color-schemes.vim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvcode-color-schemes.vim"
  },
  ["nvim-autopairs"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-autopairs"
  },
  ["nvim-colorizer.lua"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-colorizer.lua"
  },
  ["nvim-compe"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-compe"
  },
  ["nvim-lspconfig"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-lspconfig"
  },
  ["nvim-lspinstall"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-lspinstall"
  },
  ["nvim-tree.lua"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-tree.lua"
  },
  ["nvim-treesitter"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-treesitter"
  },
  ["nvim-ts-autotag"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-ts-autotag"
  },
  ["nvim-web-devicons"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/nvim-web-devicons"
  },
  ["onedark.vim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/onedark.vim"
  },
  ["packer.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/packer.nvim"
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
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/purescript-vim"
  },
  ["quick-scope"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/quick-scope"
  },
  ["telescope-fzy-native.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/telescope-fzy-native.nvim"
  },
  ["telescope.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/telescope.nvim"
  },
  ["tokyonight.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/tokyonight.nvim"
  },
  ["trouble.nvim"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/trouble.nvim"
  },
  ["vim-bbye"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/vim-bbye"
  },
  ["vim-dadbod"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/vim-dadbod"
  },
  ["vim-dadbod-ui"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/vim-dadbod-ui"
  },
  ["vim-devicons"] = {
    loaded = false,
    needs_bufread = false,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/opt/vim-devicons"
  },
  ["vim-eft"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/vim-eft"
  },
  ["vim-moonfly-colors"] = {
    loaded = true,
    path = "/home/mahdi/.local/share/nvim/site/pack/packer/start/vim-moonfly-colors"
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
if should_profile then save_profiles() end

END

catch
  echohl ErrorMsg
  echom "Error in packer_compiled: " .. v:exception
  echom "Please check your config for correctness"
  echohl None
endtry
