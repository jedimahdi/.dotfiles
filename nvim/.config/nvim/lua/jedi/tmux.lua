local api = vim.api

local send_tmux_cmd = function(cmd)
  local stdout = vim.split(vim.fn.system("tmux " .. cmd), "\n", {})
  return stdout, vim.v.shell_error
end

local get_pane_id = function()
  return send_tmux_cmd('display-message -p "#{pane_id}"')[1]
end

-- move between tmux splits and neovim windows
local tmux_directions = { h = "L", j = "D", k = "U", l = "R" }

local send_move_cmd = function(direction)
  send_tmux_cmd("selectp -" .. tmux_directions[direction])
end

local move = function(direction)
  local current_win = api.nvim_get_current_win()
  vim.cmd("wincmd " .. direction)

  if api.nvim_get_current_win() == current_win then
    send_move_cmd(direction)
  end
end

local initial_tmux_pane = function() end

local run_project = function()
  -- local ft = vim.bo.ft

  -- if ft == "typescriptreact" or ft == "typescript" then
  --   send_tmux_cmd("neww -dn npm npm run dev")
  -- end
end

-- u.nmap("<leader>tq", ":lua require'config.tmux'.run_project()<CR>")

local exec_project = function()
  local ft = vim.bo.ft
  if ft == "rust" then
    send_tmux_cmd([[neww -n cargo bash -c "cargo run; sleep 2"]])
  end

  if ft == "haskell" then
    send_tmux_cmd([[neww -n cabal bash -c "cabal run lazysh; sleep 2"]])
  end
end

local test_project = function()
  local ft = vim.bo.ft
  if ft == "typescript" then
    send_tmux_cmd([[split-window -h -p 30 -d bash -c "npx jest --watch"]])
  end

  if ft == "rust" then
    send_tmux_cmd([[neww -n cargo bash -c "cargo test; sleep 2"]])
  end

  if ft == "haskell" then
    send_tmux_cmd([[neww -n cabal bash -c "cabal test --test-show-details=always --test-option="--color" ; sleep 2"]])
  end
end

local build_project = function()
  local ft = vim.bo.ft
  if ft == "rust" then
    send_tmux_cmd([[neww -n cargo bash -c "cargo build; sleep 2"]])
  end

  if ft == "haskell" then
    send_tmux_cmd([[neww -dn cabal-build bash -c "cabal build"]])
  end
end

vim.keymap.set("n", "<C-h>", function()
  move("h")
end)
vim.keymap.set("n", "<C-j>", function()
  move("j")
end)
vim.keymap.set("n", "<C-k>", function()
  move("k")
end)
vim.keymap.set("n", "<C-l>", function()
  move("l")
end)

-- vim.keymap.set("n", "<leader>tt", exec_project)
vim.keymap.set("n", "<leader>tt", test_project)
-- u.nmap("<leader>te", ":lua require'config.tmux'.exec_project()<CR>")
-- u.nmap("<leader>tt", ":lua require'config.tmux'.test_project()<CR>")
-- u.nmap("<leader>tb", ":lua require'config.tmux'.build_project()<CR>")
