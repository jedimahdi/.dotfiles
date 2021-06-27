local home = os.getenv("HOME")
local sumneko_root_path = home..'/.local/share/lsp/lua-language-server'
local sumneko_binary = sumneko_root_path.."/bin/Linux/lua-language-server"

-- Lua
--[[ require'lspconfig'.sumneko_lua.setup {
  cmd = {sumneko_binary, "-E", sumneko_root_path .. "/main.lua"};
  settings = {
    Lua = {
      runtime = {
        version = 'LuaJIT',
        path = vim.split(package.path, ';'),
      },
      diagnostics = {
        globals = {'vim'},
      },
      workspace = {
        library = {
          [vim.fn.expand('$VIMRUNTIME/lua')] = true,
          [vim.fn.expand('$VIMRUNTIME/lua/vim/lsp')] = true,
        },
      },
    },
  },
} ]]

-- Haskell
--[[ require'lspconfig'.hls.setup {
  settings = {
    languageServerHaskell = {
      formattingProvider = "stylish-haskell"
    }
  },
  handlers = {
       ["textDocument/publishDiagnostics"] = vim.lsp.with(
         vim.lsp.diagnostic.on_publish_diagnostics, {
           underline = false,
           -- Disable virtual_text
           virtual_text = false
         }
       ),
     }
} ]]

-- Purescript
require'lspconfig'.purescriptls.setup{}

-- C++
require'lspconfig'.clangd.setup{}

-- add LspLog and LspRestart
function _G.reload_lsp()
  vim.lsp.stop_client(vim.lsp.get_active_clients())
  vim.cmd [[edit]]
end

function _G.open_lsp_log()
  local path = vim.lsp.get_log_path()
  vim.cmd("edit " .. path)
end

vim.cmd([[ command! Format execute 'lua vim.lsp.buf.formatting()' ]])
vim.cmd('command! -nargs=0 LspLog call v:lua.open_lsp_log()')
-- vim.cmd('command! -nargs=0 LspRestart call v:lua.reload_lsp()')
