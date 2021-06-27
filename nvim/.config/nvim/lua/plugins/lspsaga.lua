local saga = require 'lspsaga'

saga.init_lsp_saga({
  -- code_action_icon = '💡',
  error_sign = '▬',
  warn_sign = '▴',
  hint_sign = '⬧',
  infor_sign = '🛈',
  code_action_prompt = {
    enable = false,
    -- sign = true,
  },
})

-- error_sign = '',
-- warn_sign = '',
-- hint_sign = '',
-- infor_sign = '',
-- dianostic_header_icon = '   ',
-- code_action_icon = ' ',
