local u = require("config.utils")

require("fzf-lua").setup({
  keymap = {
    fzf = {
      ["ctrl-q"] = "select-all+accept",
    },
  },
  winopts = {
    height = 0.95,
    width = 0.95,
    preview = {
      scrollbar = false,
    },
  },
  fzf_opts = {
    ["--layout"] = "default",
  },
  previewers = {
    git_diff = {
      pager = "delta",
    },
  },
})

-- fzf.vim-like commands
u.command("Files", "FzfLua files")
u.command("Rg", "FzfLua live_grep_resume")
u.command("BLines", "FzfLua grep_curbuf")
-- u.command("GrepCword", "FzfLua grep_cWORD")
u.command("History", "FzfLua oldfiles")
u.command("Buffers", "FzfLua buffers")
u.command("BCommits", "FzfLua git_bcommits")
u.command("Commits", "FzfLua git_commits")
u.command("HelpTags", "FzfLua help_tags")
u.command("ManPages", "FzfLua man_pages")
u.command("GrepClipboard", function()
  -- remove newlines, since they'll break the search
  local search = vim.fn.getreg("*"):gsub("\n", "")
  require("fzf-lua").grep({ search = search })
end)

u.nmap("<Leader>f", "<cmd>Files<CR>")
u.nmap("<Leader>sw", "<cmd>Rg<CR>")
u.nmap("<Leader>sh", "<cmd>HelpTags<CR>")
u.nmap("<Leader>sr", "<cmd>History<CR>")
u.nmap("<Leader>ss", "<cmd>FzfLua grep_cword<CR>")
u.nmap("<Leader>sS", "<cmd>FzfLua grep_cWORD<CR>")
u.nmap("<Leader>sg", "<cmd>GrepClipboard<CR>")
u.nmap("<Leader>sl", "<cmd>BLines<CR>")
u.nmap("<Leader>sc", "<cmd>BCommits<CR>")
u.nmap("<Leader>sb", "<cmd>Buffers<CR>")
u.nmap("<Leader>tc", "<cmd>FzfLua colorschemes<CR>")

u.nmap("<Leader>gs", "<cmd>FzfLua git_status<CR>")
u.nmap("<Leader>gb", "<cmd>FzfLua git_branches<CR>")

u.command("LspRef", "FzfLua lsp_references")
u.command("LspSym", "FzfLua lsp_workspace_symbols")
u.command("LspAct", "FzfLua lsp_code_actions")
u.command("LspDef", function()
  require("fzf-lua").lsp_definitions({ jump_to_single_result = true })
end)
