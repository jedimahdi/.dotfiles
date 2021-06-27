local actions = require('telescope.actions')
require('telescope').setup {
  defaults = {
    prompt_prefix = '🔭 ',
    prompt_position = 'top',
    initial_mode = 'insert',
    sorting_strategy = 'ascending',
    results_width = 0.6,
    file_previewer = require'telescope.previewers'.vim_buffer_cat.new,
    grep_previewer = require'telescope.previewers'.vim_buffer_vimgrep.new,
    qflist_previewer = require'telescope.previewers'.vim_buffer_qflist.new,

    mappings = {
      i = {
        ['<C-j>'] = actions.move_selection_next,
        ['<C-k>'] = actions.move_selection_previous,
        ['<Esc>'] = actions.close,
      },
      n = {
        ['<C-j>'] = actions.move_selection_next,
        ['<C-k>'] = actions.move_selection_previous,
        ['<Esc>'] = actions.close,
      },
    },
  },
  extensions = { fzy_native = { override_generic_sorter = false, override_file_sorter = true } },
}
require('telescope').load_extension('fzy_native')

-- require('telescope').load_extension('fzy_native')
-- require'telescope'.load_extension('dotfiles')
-- require'telescope'.load_extension('gosource')
