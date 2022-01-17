local u = require("utils")

require("fzf-lua").setup({
  winopts = {
    preview = {
      scrollbar = false,
    },
  },
  fzf_opts = {
    ["--layout"] = "default",
  },
  files = {
    actions = {
      ["ctrl-f"] = function(selected)
        vim.cmd(string.format("edit %s | TestFile Vsplit", selected[1]))
      end,
      ["ctrl-v"] = function(selected)
        vim.cmd("Vsplit " .. selected[1])
      end,
      ["ctrl-s"] = function(selected)
        vim.cmd("Split " .. selected[1])
      end,
    },
  },
})

-- fzf.vim-like commands
u.command("Files", "FzfLua files")
u.command("Rg", "FzfLua live_grep_resume")
u.command("BLines", "FzfLua grep_curbuf")
u.command("History", "FzfLua oldfiles")
u.command("Buffers", "FzfLua buffers")
u.command("BCommits", "FzfLua git_bcommits")
u.command("Commits", "FzfLua git_commits")
u.command("HelpTags", "FzfLua help_tags")
u.command("ManPages", "FzfLua man_pages")

u.nmap("<Leader>f", "<cmd>Files<CR>")
u.nmap("<Leader>w", "<cmd>Rg<CR>")
u.nmap("<Leader>so", "<cmd>History<CR>")
u.nmap("<Leader>sh", "<cmd>HelpTags<CR>")
u.nmap("<Leader>sl", "<cmd>BLines<CR>")
u.nmap("<Leader>sc", "<cmd>BCommits<CR>")
u.nmap("<Leader>b", "<cmd>Buffers<CR>")

u.lua_command("LspDef", "require('fzf-lua').lsp_definitions({ jump_to_single_result = true })")
u.command("LspRef", "FzfLua lsp_references")
u.command("LspSym", "FzfLua lsp_workspace_symbols")
u.command("LspAct", "FzfLua lsp_code_actions")
