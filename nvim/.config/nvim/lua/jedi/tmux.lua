local api = vim.api

-- shared
local send_tmux_cmd = function(cmd)
  local stdout = vim.split(vim.fn.system("tmux " .. cmd), "\n")
  return stdout, vim.v.shell_error
end

local get_pane_id = function()
  return send_tmux_cmd('display-message -p "#{pane_id}"')[1]
end

local M = {}

-- move between tmux splits and neovim windows
local tmux_directions = { h = "L", j = "D", k = "U", l = "R" }

local send_move_cmd = function(direction)
  send_tmux_cmd("selectp -" .. tmux_directions[direction])
end

M.move = function(direction)
  local current_win = api.nvim_get_current_win()
  vim.cmd("wincmd " .. direction)

  if api.nvim_get_current_win() == current_win then
    send_move_cmd(direction)
  end
end

M.run_project = function()
  -- local ft = vim.bo.ft

  -- if ft == "typescriptreact" or ft == "typescript" then
  --   send_tmux_cmd("neww -dn npm npm run dev")
  -- end
end

-- u.nmap("<leader>tq", ":lua require'config.tmux'.run_project()<CR>")

M.exec_project = function()
  local ft = vim.bo.ft
  if ft == "rust" then
    send_tmux_cmd([[neww -n cargo bash -c "cargo run; sleep 2"]])
  end

  if ft == "haskell" then
    send_tmux_cmd([[neww -n cabal bash -c "cabal run lazysh; sleep 2"]])
  end
end

M.test_project = function()
  local ft = vim.bo.ft
  if ft == "rust" then
    send_tmux_cmd([[neww -n cargo bash -c "cargo test; sleep 2"]])
  end

  if ft == "haskell" then
    send_tmux_cmd([[neww -n cabal bash -c "cabal test --test-show-details=always --test-option="--color" ; sleep 2"]])
  end
end

M.build_project = function()
  local ft = vim.bo.ft
  if ft == "rust" then
    send_tmux_cmd([[neww -n cargo bash -c "cargo build; sleep 2"]])
  end

  if ft == "haskell" then
    send_tmux_cmd([[neww -dn cabal-build bash -c "cabal build"]])
  end
end

-- u.nmap("<leader>te", ":lua require'config.tmux'.exec_project()<CR>")
-- u.nmap("<leader>tt", ":lua require'config.tmux'.test_project()<CR>")
-- u.nmap("<leader>tb", ":lua require'config.tmux'.build_project()<CR>")

return M
