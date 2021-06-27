local home = os.getenv('HOME')
local cache_dir = home .. '/.cache/nvim/'

local function apply_options(opts)
  for k, v in pairs(opts) do
    if v == true then
      vim.cmd('set ' .. k)
    elseif v == false then
      vim.cmd(string.format('set no%s', k))
    else
      vim.cmd(string.format('set %s=%s', k, v))
    end
  end
end

apply_options {
  termguicolors = true,
  mouse = 'a',
  relativenumber = true,
  number = true,
  hidden = true,
  fileformats = 'unix,mac,dos',
  magic = true,
  virtualedit = 'block',
  encoding = 'utf-8',
  viewoptions = 'folds,cursor,curdir,slash,unix',
  sessionoptions = 'curdir,help,tabpages,winsize',
  clipboard = 'unnamed',
  -- clipboard      = "unnamedplus",
  wildignorecase = true,
  wildignore = '.git,.hg,.svn,*.pyc,*.o,*.out,*.jpg,*.jpeg,*.png,*.gif,*.zip,**/tmp/**,*.DS_Store,**/node_modules/**,**/bower_modules/**',
  backup = false,
  writebackup = false,
  undofile = true,
  swapfile = false,
  directory = cache_dir .. 'swag/',
  undodir = cache_dir .. 'undo/',
  backupdir = cache_dir .. 'backup/',
  viewdir = cache_dir .. 'view/',
  spellfile = cache_dir .. 'spell/en.uft-8.add',
  history = 2000,
  shada = '!,\'300,<50,@100,s10,h',
  smarttab = true,
  shiftround = true,
  updatetime = 100,
  redrawtime = 1500,
  ignorecase = true,
  smartcase = true,
  infercase = true,
  incsearch = true,
  wrapscan = true,
  complete = '.,w,b,k',
  inccommand = 'nosplit',
  grepformat = '%f:%l:%c:%m',
  -- grepprg        = "rg --hidden --vimgrep --smart-case --",
  breakat = [[\ \	;:,!?]],
  startofline = false,
  whichwrap = 'h,l,<,>,[,],~',
  splitbelow = true,
  splitright = true,
  switchbuf = 'useopen',
  backspace = 'indent,eol,start',
  diffopt = 'filler,iwhite,internal,algorithm:patience',
  completeopt = 'menuone,noselect',
  jumpoptions = 'stack',
  showmode = false,
  shortmess = 'aoOTIcF',
  scrolloff = 2,
  sidescrolloff = 5,
  foldlevelstart = 99,
  ruler = false,
  list = false,
  winwidth = 30,
  winminwidth = 10,
  pumheight = 15,
  helpheight = 12,
  cmdheight = 1,
  cmdwinheight = 5,
  showcmd = false,
  equalalways = false,
  laststatus = 2,
  showbreak = '↳  ',
  listchars = 'tab:»·,nbsp:+,trail:·,extends:→,precedes:←',
  signcolumn = 'yes',
  pumblend = 10,
  winblend = 10,
  tabstop = 2,
  shiftwidth = 2,
  -- softtabstop    = 2,
  softtabstop = -1,
  textwidth = 120,
  expandtab = true,
  autoindent = true,
  formatoptions = '1jcroql',
  breakindentopt = 'shift:2,min:20',
  wrap = true,
  linebreak = true,
  foldenable = false,
  hlsearch = true,
}
-- vim.cmd("colorscheme onedark")
vim.cmd('colorscheme zephyr')
-- vim.cmd[[colorscheme tokyonight]]
-- vim.g.onedark_style = 'deep'
