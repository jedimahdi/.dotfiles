local u = require("config.utils")

local gitsigns = require("gitsigns")

require("gitsigns").setup()

gitsigns.setup({
  on_attach = function(bufnr)
    local gs = package.loaded.gitsigns

    local function map(mode, l, r, opts)
      opts = opts or {}
      opts.buffer = bufnr
      vim.keymap.set(mode, l, r, opts)
    end

    -- Navigation
    map("n", "]c", function()
      if vim.wo.diff then
        return "]c"
      end
      vim.schedule(function()
        gs.next_hunk()
      end)
      return "<Ignore>"
    end, { expr = true })

    map("n", "[c", function()
      if vim.wo.diff then
        return "[c"
      end
      vim.schedule(function()
        gs.prev_hunk()
      end)
      return "<Ignore>"
    end, { expr = true })

    -- Actions
    map({ "n", "v" }, "<leader>hs", ":Gitsigns stage_hunk<CR>")
    map({ "n", "v" }, "<leader>hr", ":Gitsigns reset_hunk<CR>")
    map("n", "<leader>hS", gs.stage_buffer)
    map("n", "<leader>hu", gs.undo_stage_hunk)
    map("n", "<leader>hR", gs.reset_buffer)
    map("n", "<leader>hp", gs.preview_hunk)
    map("n", "<leader>hb", function()
      gs.blame_line({ full = true })
    end)
    map("n", "<leader>tb", gs.toggle_current_line_blame)
    map("n", "<leader>hd", gs.diffthis)
    map("n", "<leader>hD", function()
      gs.diffthis("~")
    end)
    map("n", "<leader>td", gs.toggle_deleted)

    -- Text object
    map({ "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>")

    -- actions
    -- u.buf_map(bufnr, { "n", "v" }, "<leader>gs", gitsigns.stage_hunk)
    -- u.buf_map(bufnr, { "n", "v" }, "<leader>gr", gitsigns.reset_hunk)
    -- u.buf_map(bufnr, "n", "<leader>hS", gitsigns.stage_buffer)
    -- u.buf_map(bufnr, "n", "<leader>hu", gitsigns.undo_stage_hunk)
    -- u.buf_map(bufnr, "n", "<leader>hR", gitsigns.reset_buffer)
    -- u.buf_map(bufnr, "n", "<leader>gp", gitsigns.preview_hunk)
    -- u.buf_map(bufnr, "n", "<leader>gb", function()
    --   gitsigns.blame_line({ full = true })
    -- end)
    --
    -- -- text object
    -- u.buf_map(bufnr, { "o", "x" }, "ih", ":<C-U>Gitsigns select_hunk<CR>")
  end,
})

-- u.nmap("<leader>gb", "<cmd>Telescope git_branches<CR>")
-- u.nmap("<leader>go", "<cmd>Telescope git_status<CR>")
--
-- u.nmap("<Leader>G", ":tab Git<CR>")
-- u.nmap("<Leader>g", ":Git ", { silent = false })

vim.cmd("autocmd FileType fugitive nmap <buffer> <Tab> =")
