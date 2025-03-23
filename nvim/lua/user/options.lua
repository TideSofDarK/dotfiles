vim.g.mapleader = ' '
vim.g.maplocalleader = '\\'

vim.g.loaded_netrw = 1
vim.g.loaded_netrwPlugin = 1

vim.o.hlsearch = false
vim.wo.number = true
vim.wo.relativenumber = true
vim.o.mouse = 'nv'

vim.schedule(function()
  vim.opt.clipboard = 'unnamedplus'
end)

-- vim.o.breakindent = false
-- vim.o.autoindent = false
-- vim.o.smartindent = false
-- vim.o.cindent = false

vim.o.hidden = true
vim.o.errorbells = false
vim.o.swapfile = false
vim.o.backup = false
vim.o.undofile = true
vim.o.backspace = "indent,eol,start"
vim.o.splitright = true
vim.o.splitbelow = true
vim.o.autochdir = false
-- vim.o.iskeyword:append("-")
vim.o.incsearch = true
vim.o.ignorecase = false
vim.o.smartcase = true
-- vim.o.colorcolumn = "100"
vim.o.signcolumn = "yes"
-- vim.o.cursorline = true
vim.o.cmdheight = 1
-- vim.o.updatetime = 250
-- vim.o.timeoutlen = 300
vim.o.completeopt = 'menu,menuone,popup,fuzzy'
vim.o.shortmess = 'OtI'
vim.o.encoding = "UTF-8"
vim.o.wrap = false
vim.o.scrolloff = 8
vim.o.sidescrolloff = 8
vim.o.showmode = false
vim.o.termguicolors = true
-- vim.o.fillchars = "vert:▕"
-- vim.o.fillchars = "vert:█"
-- vim.o.fillchars = "vert:┃"
-- vim.o.fillchars = "vert:▐"
-- vim.o.fillchars = "vert:╎"

vim.o.tabstop = 8
vim.o.softtabstop = 4
vim.o.shiftwidth = 4

if vim.fn.executable("pwsh") == 1 then
  vim.opt.shell = "pwsh"
end
