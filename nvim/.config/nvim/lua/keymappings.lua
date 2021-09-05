vim.g.mapleader = ','

-- explorer
-- vim.api.nvim_set_keymap('n', '<leader>e', ':NvimTreeToggle<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>F', ':NvimTreeFindFile<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>.', '<C-^>', { noremap = true, silent = true })

-- better window movement
vim.api.nvim_set_keymap('n', '<C-h>', '<C-w>h', { silent = true })
vim.api.nvim_set_keymap('n', '<C-j>', '<C-w>j', { silent = true })
vim.api.nvim_set_keymap('n', '<C-k>', '<C-w>k', { silent = true })
vim.api.nvim_set_keymap('n', '<C-l>', '<C-w>l', { silent = true })

-- resize with arrows
vim.api.nvim_set_keymap('n', '<C-Up>', ':resize -2<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<C-Down>', ':resize +2<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<C-Left>', ':vertical resize -2<CR>', { silent = true })
vim.api.nvim_set_keymap('n', '<C-Right>', ':vertical resize +2<CR>', { silent = true })

-- better indenting
vim.api.nvim_set_keymap('v', '<', '<gv', { noremap = true, silent = true })
vim.api.nvim_set_keymap('v', '>', '>gv', { noremap = true, silent = true })

-- Tab switch buffer
vim.api.nvim_set_keymap('n', '<TAB>', ':bnext<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<S-TAB>', ':bprevious<CR>', { noremap = true, silent = true })

-- Move selected line / block of text in visual mode
vim.api.nvim_set_keymap('x', 'K', ':move \'<-2<CR>gv-gv', { noremap = true, silent = true })
vim.api.nvim_set_keymap('x', 'J', ':move \'>+1<CR>gv-gv', { noremap = true, silent = true })

-- Higlight
vim.api.nvim_set_keymap('n', '<space>', ':set hlsearch! hlsearch?<CR>', { noremap = true, silent = true })

-- Telescope
-- vim.api.nvim_set_keymap('n', '<leader>f', ':Telescope find_files<CR>', { noremap = true, silent = true })
-- vim.api.nvim_set_keymap('n', '<leader>g', ':Telescope git_files<CR>', { noremap = true, silent = true })
-- vim.api.nvim_set_keymap('n', '<leader>w', ':Telescope live_grep<CR>', { noremap = true, silent = true })
-- vim.api.nvim_set_keymap('n', '<leader>c', ':Telescope colorscheme<CR>', { noremap = true, silent = true })
-- vim.api.nvim_set_keymap('n', '<leader>r', ':Telescope oldfiles<CR>', { noremap = true, silent = true })

-- LSP
vim.api.nvim_set_keymap('n', 'gd', ':lua vim.lsp.buf.definition()<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'gD', ':Lspsaga preview_definition<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'gi', ':lua vim.lsp.buf.implementation()<CR>', { noremap = true, silent = true })
-- vim.api.nvim_set_keymap('n', 'gr', ':lua vim.lsp.buf.references()<CR>', {noremap = true, silent = true})
vim.api.nvim_set_keymap('n', 'gn', ':lua vim.lsp.buf.rename()<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'gh', ':Lspsaga lsp_finder<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'gs', ':lua vim.lsp.buf.signature_help()<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>a', ':lua vim.lsp.buf.code_action()<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>s', ':Lspsaga show_line_diagnostics<CR>', { noremap = true, silent = true })
-- vim.api.nvim_set_keymap('n', 'K', ':lua vim.lsp.buf.hover()<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', 'K', ':Lspsaga hover_doc<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<C-f>', '<cmd>lua require(\'lspsaga.action\').smart_scroll_with_saga(1)<CR>',
                        { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<C-b>', '<cmd>lua require(\'lspsaga.action\').smart_scroll_with_saga(-1)<CR>',
                        { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '[e', ':Lspsaga diagnostic_jump_next<CR>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', ']e', ':Lspsaga diagnostic_jump_prev<CR>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<leader>p', '<cmd>lua vim.lsp.buf.formatting()<CR>', { noremap = true, silent = true })

-- Trouble
--[[ vim.api.nvim_set_keymap('n', '<leader>xx', '<cmd>Trouble<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<leader>xw', '<cmd>Trouble lsp_workspace_diagnostics<cr>',
                        { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<leader>xd', '<cmd>Trouble lsp_document_diagnostics<cr>',
                        { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<leader>xl', '<cmd>Trouble loclist<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', '<leader>xq', '<cmd>Trouble quickfix<cr>', { silent = true, noremap = true })
vim.api.nvim_set_keymap('n', 'gr', '<cmd>Trouble lsp_references<cr>', { silent = true, noremap = true }) ]]

-- Format
-- vim.api.nvim_set_keymap('n', '<leader>p', ':Neoformat<CR>', {noremap = true, silent = true})

-- Other
vim.api.nvim_set_keymap('n', '<leader>q', ':Bdelete<CR>', { noremap = true, silent = true })
vim.api.nvim_set_keymap('n', '<leader>z', ':Neogit<CR>', { noremap = true, silent = true })

--[[ vim.cmd('nmap j <Plug>(accelerated_jk_gj)')
vim.cmd('nmap k <Plug>(accelerated_jk_gk)') ]]

vim.cmd('command Q q')
vim.cmd('command W w')

local which_key_ok, which_key = pcall(require, 'which-key')
if not which_key_ok then return end

which_key.register({
  e = { '<cmd>NvimTreeToggle<cr>', 'Explorer' },
  f = { '<cmd>Telescope find_files<cr>', 'Find File' },
  d = {
    name = 'Diagnostics',
    t = { '<cmd>TroubleToggle<cr>', 'trouble' },
    w = { '<cmd>TroubleToggle lsp_workspace_diagnostics<cr>', 'workspace' },
    d = { '<cmd>TroubleToggle lsp_document_diagnostics<cr>', 'document' },
    q = { '<cmd>TroubleToggle quickfix<cr>', 'quickfix' },
    l = { '<cmd>TroubleToggle loclist<cr>', 'loclist' },
    r = { '<cmd>TroubleToggle lsp_references<cr>', 'references' },
  },
  g = {
    name = 'Git',
    o = { '<cmd>Telescope git_status<cr>', 'Open changed file' },
    b = { '<cmd>Telescope git_branches<cr>', 'Checkout branch' },
    c = { '<cmd>Telescope git_commits<cr>', 'Checkout commit' },
    C = { '<cmd>Telescope git_bcommits<cr>', 'Checkout commit(for current file)' },
  },
  l = {
    name = 'LSP',
    a = { '<cmd>Lspsaga code_action<cr>', 'Code Action' },
    A = { '<cmd>Lspsaga range_code_action<cr>', 'Selected Action' },
    d = { '<cmd>Telescope lsp_document_diagnostics<cr>', 'Document Diagnostics' },
    D = { '<cmd>Telescope lsp_workspace_diagnostics<cr>', 'Workspace Diagnostics' },
    f = { '<cmd>LspFormatting<cr>', 'Format' },
    h = { '<cmd>Lspsaga hover_doc<cr>', 'Hover Doc' },
    i = { '<cmd>LspInfo<cr>', 'Info' },
    L = { '<cmd>Lspsaga lsp_finder<cr>', 'LSP Finder' },
    l = { '<cmd>Lspsaga show_line_diagnostics<cr>', 'Line Diagnostics' },
    p = { '<cmd>Lspsaga preview_definition<cr>', 'Preview Definition' },
    q = { '<cmd>Telescope quickfix<cr>', 'Quickfix' },
    r = { '<cmd>Lspsaga rename<cr>', 'Rename' },
    t = { '<cmd>LspTypeDefinition<cr>', 'Type Definition' },
    x = { '<cmd>cclose<cr>', 'Close Quickfix' },
    s = { '<cmd>Telescope lsp_document_symbols<cr>', 'Document Symbols' },
    S = { '<cmd>Telescope lsp_dynamic_workspace_symbols<cr>', 'Workspace Symbols' },
  },
  r = {
    name = 'Replace',
    f = { '<cmd>lua require(\'spectre\').open_file_search()<cr>', 'Current File' },
    p = { '<cmd>lua require(\'spectre\').open()<cr>', 'Project' },
  },
  t = {
    name = 'Search',
    b = { '<cmd>Telescope git_branches<cr>', 'Checkout branch' },
    c = { '<cmd>Telescope colorscheme<cr>', 'Colorscheme' },
    d = { '<cmd>Telescope lsp_document_diagnostics<cr>', 'Document Diagnostics' },
    D = { '<cmd>Telescope lsp_workspace_diagnostics<cr>', 'Workspace Diagnostics' },
    f = { '<cmd>Telescope find_files<cr>', 'Find File' },
    h = { '<cmd>Telescope help_tags<cr>', 'Find Help' },
    m = { '<cmd>Telescope marks<cr>', 'Marks' },
    M = { '<cmd>Telescope man_pages<cr>', 'Man Pages' },
    r = { '<cmd>Telescope oldfiles<cr>', 'Open Recent File' },
    R = { '<cmd>Telescope registers<cr>', 'Registers' },
    t = { '<cmd>Telescope live_grep<cr>', 'Text' },
  },
  --[[ f = {
    name = 'find',
    b = { '<cmd>lua require(\'telescope.builtin\').buffers()<cr>', 'buffers' },
    f = { '<cmd>lua require(\'telescope.builtin\').find_files({follow = true})<cr>', 'files' },
    g = { '<cmd>lua require(\'telescope.builtin\').live_grep()<cr>', 'whit live grep' },
    h = { '<cmd>lua require(\'telescope.builtin\').help_tags()<cr>', 'help tags' },
  }, ]]
}, {
  mode = 'n', -- NORMAL mode
  prefix = '<leader>',
  buffer = nil, -- Global mappings. Specify a buffer number for buffer local mappings
  silent = true, -- use `silent` when creating keymaps
  noremap = true, -- use `noremap` when creating keymaps
  nowait = false, -- use `nowait` when creating keymaps
})
