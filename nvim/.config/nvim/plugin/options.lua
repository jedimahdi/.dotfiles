local opt = vim.opt

-- NOTE: If on, it can erase a message ouput by `:echo` or `:lua print`
opt.showmode = false

opt.showcmd = true
opt.cmdheight = 1
opt.incsearch = true -- Makes search act like search in modern browsers
opt.showmatch = true -- show matching brackets when text indicator is over them
opt.number = false
opt.relativenumber = false
opt.ignorecase = true -- Ignore case when searching...
opt.smartcase = true -- ... unless there is a capital letter in the query
opt.hidden = true -- I like having buffers stay around
opt.cursorline = true -- Highlight the current line
opt.splitright = true -- Prefer windows splitting to the right
opt.splitbelow = true -- Prefer windows splitting to the bottom
opt.updatetime = 1000 -- Make updates happen faster
opt.hlsearch = true -- I wouldn't use this without my DoNoHL function
opt.scrolloff = 8 -- Make it so there are always ten lines below my cursor

-- Ignore compiled files
opt.wildignore = "__pycache__"
opt.wildignore = opt.wildignore + { "*.o", "*~", "*.pyc", "*pycache*" }

-- Cool floating window popup menu for completion on command line
opt.pumheight = 15
opt.pumblend = 17 -- tranceparency
opt.wildmode = {
  -- complete longest common string : show the wildmenu
  "longest:full",

  -- start completing each full match
  "full",
}
opt.wildoptions = "pum"

-- Tabs
opt.autoindent = true
opt.cindent = true
opt.wrap = true

opt.tabstop = 2
opt.shiftwidth = 2
opt.softtabstop = 2
opt.expandtab = true
opt.smarttab = true

opt.breakindent = true
opt.showbreak = string.rep(" ", 3) -- Make it so that long lines wrap smartly
opt.linebreak = true

opt.foldmethod = "marker"
opt.foldlevel = 0
opt.modelines = 1

opt.belloff = "all" -- Just turn the dang bell off

opt.clipboard = "unnamedplus" -- unnamed

opt.swapfile = false -- Living on the edge

opt.mouse = "a"

opt.formatoptions = opt.formatoptions
  - "a" -- Auto formatting is BAD.
  - "t" -- Don't auto format my code. I got linters for that.
  + "c" -- In general, I like it when comments respect textwidth
  + "q" -- Allow formatting comments w/ gq
  - "o" -- O and o, don't continue comments
  - "r" -- But do continue when pressing enter.
  + "n" -- Indent past the formatlistpat, not underneath it.
  + "j" -- Auto-remove comments if possible.
  - "2" -- I'm not in gradeschool anymore

opt.joinspaces = false -- Two spaces and grade school, we're done

opt.shortmess:append({
  -- Enable all sorts of abbreviations in messages
  a = true,

  -- Don't print |ins-completion-menu| messages. For example: {{{
  --
  --    - "-- XXX completion (YYY)"
  --    - "match 1 of 2"
  --    - "The only match"
  --    - "Pattern not found"
  --    - "Back at original"
  -- }}}
  c = true,

  -- Disable the default Vim startup message
  I = true,
})

opt.magic = true
vim.o.termguicolors = true
vim.opt.termguicolors = true
opt.undofile = true
opt.sidescrolloff = 2
opt.timeoutlen = 800
opt.directory = CACHE_PATH .. "/swag/"
opt.undodir = CACHE_PATH .. "/undo/"
opt.backupdir = CACHE_PATH .. "/backup/"
opt.viewdir = CACHE_PATH .. "/view/"
opt.spellfile = CACHE_PATH .. "/spell/en.uft-8.add"
opt.backup = false
opt.writebackup = false
opt.undofile = true
opt.encoding = "utf-8"
opt.statusline = [[%f %y %m %= %p%% %l:%c]]
opt.laststatus = 0
opt.ruler = false

opt.list = true
opt.listchars = {
  -- TAB character
  --
  --     ┌ always used
  --     │┌ as many times as will fit
  tab = "▸ ",

  -- no-break space
  nbsp = "∅",

  -- trailing whitespace
  trail = "·",

  -- end of line (it's annoying to display all the time)
  -- eol = "↴",
}

opt.fillchars = {
  -- Don't print '~' at the start of the lines after the last buffer line
  eob = " ",

  -- Fill 'foldtext' with simple dots instead of hyphens
  fold = "·",

  -- Replace the ugly default icons '+' and '-' with prettier utf8 characters.
  -- These are only visible in the `foldcolumn`
  foldclose = "▸",
  foldopen = "▾",
  foldsep = "│",

  -- Used for vertical splits
  -- Alternatives: "┃", "│", "▕"
  vert = "│",
}

-- A margin between the left of the screen and the text to display signs
--
-- Used by:
--
--   - builtin-in lsp
--   - gitsigns.nvim
opt.signcolumn = "yes:1"
