require("ocaml").setup()

require("nvim-treesitter.configs").setup({
  ensure_installed = {
    -- "c",
    "lua",
    -- "rust",
    -- "haskell",
    -- "json",
    -- "javascript",
    -- "nix",
    -- "zig",
    -- "yaml",
    -- "typescript",
    -- "tsx",
    -- "toml",
    -- "sql",
    -- "python",
    -- "prisma",
    -- "markdown",
    -- "latex",
    -- "html",
    -- "go",
    -- "fish",
    -- "elm",
    -- "css",
    -- "cpp",
    -- "bash",
    -- "ocaml",
    -- "rapper",
  },

  highlight = {
    enable = true,
    disable = {},
  },
  indent = {
    enable = true,
    disable = {},
  },
  autotag = {
    enable = true,
  },
  incremental_selection = {
    enable = true,
    keymaps = {
      init_selection = "<C-Space>", -- set to `false` to disable one of the mappings
      node_incremental = "<C-Space>",
      scope_incremental = false,
      node_decremental = "<BS>",
    },
  },

  playground = {
    enable = true,
    disable = {},
    updatetime = 25, -- Debounced time for highlighting nodes in the playground from source code
    persist_queries = false, -- Whether the query persists across vim sessions
    keybindings = {
      toggle_query_editor = "o",
      toggle_hl_groups = "i",
      toggle_injected_languages = "t",
      toggle_anonymous_nodes = "a",
      toggle_language_display = "I",
      focus_language = "f",
      unfocus_language = "F",
      update = "R",
      goto_node = "<cr>",
      show_help = "?",
    },
  },
})

local parser_config = require("nvim-treesitter.parsers").get_parser_configs()
parser_config.tsx.filetype_to_parsername = { "javascript", "typescript.tsx" }

-- parser_config.jedi = {
--   install_info = {
--     url = "~/code/tree-sitter-jedi", -- local path or git repo
--     files = { "src/parser.c" },
--     -- optional entries:
--     branch = "main", -- default branch in case of git repo if different from master
--     generate_requires_npm = false, -- if stand-alone parser without npm dependencies
--     requires_generate_from_grammar = false, -- if folder contains pre-generated src/parser.c
--   },
--   filetype = "jedi", -- if filetype does not match the parser name
-- }
--
-- parser_config.alex = {
--   install_info = {
--     url = "~/code/tree-sitter-alex",
--     files = { "src/parser.c", "src/scanner.c" },
--   },
--   filetype = "alex",
-- }
--
-- parser_config.purescript = {
--   install_info = {
--     url = "~/code/tree-sitter-purescript",
--     files = { "src/parser.c" },
--   },
--   filetype = "purescript",
-- }
--
-- vim.cmd("autocmd BufNewFile,BufRead *.x setf alex")
-- vim.cmd("autocmd BufNewFile,BufRead *.purs setf purescript")
