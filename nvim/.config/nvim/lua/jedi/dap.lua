local dap = require("dap")
local dapui = require("dapui")

require("nvim-dap-virtual-text").setup()
-- dapui.setup()

-- dap.listeners.after.event_initialized["dapui_config"] = function()
--   dapui.open()
-- end
-- dap.listeners.before.event_terminated["dapui_config"] = function()
--   dapui.close()
-- end
-- dap.listeners.before.event_exited["dapui_config"] = function()
--   dapui.close()
-- end

vim.keymap.set("n", "<leader>lu", function()
  local widgets = require("dap.ui.widgets")
  local sidebar = widgets.sidebar(widgets.scopes)
  sidebar.toggle()
end, { desc = "Toggle Debug UI" })
vim.keymap.set("n", "<leader>lb", "<cmd>DapToggleBreakpoint<CR>", { desc = "BreakPoint" })
vim.keymap.set("n", "<leader>ls", "<cmd>DapStepOver<CR>", { desc = "Step Over" })
vim.keymap.set("n", "<leader>li", "<cmd>DapStepInto<CR>", { desc = "Step Into" })
vim.keymap.set("n", "<leader>lc", "<cmd>DapContinue<CR>", { desc = "Continue" })

vim.keymap.set("n", "<leader>db", "<cmd>DapToggleBreakpoint<CR>", { desc = "Debug Breakpoint" })
vim.keymap.set("n", "<leader>dr", "<cmd>DapContinue<CR>", { desc = "Run Breakpoint" })

dap.adapters["pwa-node"] = {
  type = "server",
  host = "localhost",
  port = "8123",
  executable = {
    command = "node",
    args = { "/home/mahdi/tmp/js-debug/src/dapDebugServer.js", "8123" },
  },
}
dap.configurations.javascript = {
  {
    type = "pwa-node",
    request = "launch",
    name = "Launch file",
    program = "${file}",
    cwd = "${workspaceFolder}",
    runtimeExecutable = "node",
  },
}
dap.configurations.typescript = {
  {
    type = 'pwa-node',
    request = 'launch',
    name = "Launch file",
    runtimeExecutable = "deno",
    runtimeArgs = {
      "run",
      "--inspect-wait",
      "--allow-all"
    },
    program = "${file}",
    cwd = "${workspaceFolder}",
    attachSimplePort = 9229,
  },
}

dap.adapters.gdb = {
  type = "executable",
  command = "gdb",
  args = { "-i", "dap" }
}

dap.configurations.c = {
  {
    name = "Launch",
    type = "gdb",
    request = "launch",
    program = "/home/mahdi/tmp/dap-demo/a.out",
    cwd = "${workspaceFolder}",
  },
}

dap.adapters.haskell = {
  type = 'executable';
  command = 'haskell-debug-adapter';
  args = {'--hackage-version=0.0.33.0'};
}
dap.configurations.haskell = {
  {
    type = 'haskell',
    request = 'launch',
    name = 'Debug',
    workspace = '${workspaceFolder}',
    startup = "${file}",
    stopOnEntry = true,
    logFile = vim.fn.stdpath('data') .. '/haskell-dap.log',
    logLevel = 'WARNING',
    ghciEnv = vim.empty_dict(),
    ghciPrompt = "λ: ",
    -- Adjust the prompt to the prompt you see when you invoke the stack ghci command below 
    ghciInitialPrompt = "λ: ",
    ghciCmd= "cabal exec -- ghci-dap --interactive -i -i${workspaceFolder}/src",
  },
}

-- dap.adapters.lldb = {
--   type = "executable",
--   command = "/usr/bin/lldb-vscode", -- adjust as needed, must be absolute path
--   name = "lldb",
-- }

-- dap.configurations.c = {
--   {
--     name = "Launch",
--     type = "lldb",
--     request = "launch",
--     program = function()
--       return vim.fn.input("Path to executable: ", vim.fn.getcwd() .. "/", "file")
--     end,
--     cwd = "${workspaceFolder}",
--     stopOnEntry = false,
--     args = {},
--
--     -- 💀
--     -- if you change `runInTerminal` to true, you might need to change the yama/ptrace_scope setting:
--     --
--     --    echo 0 | sudo tee /proc/sys/kernel/yama/ptrace_scope
--     --
--     -- Otherwise you might get the following error:
--     --
--     --    Error on launch: Failed to attach to the target process
--     --
--     -- But you should be aware of the implications:
--     -- https://www.kernel.org/doc/html/latest/admin-guide/LSM/Yama.html
--     -- runInTerminal = false,
--   },
-- }

-- dap.configurations.cpp = dap.configurations.c
