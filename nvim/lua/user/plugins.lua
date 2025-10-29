vim.pack.add({
  "https://github.com/tpope/vim-fugitive",
  "https://github.com/tpope/vim-sleuth",
  "https://github.com/rebelot/kanagawa.nvim"
})

vim.cmd [[colorscheme kanagawa]]

require('user/plugins/mini')
require('user/plugins/blink')
require('user/plugins/gitsigns')
require('user/plugins/conform')
require('user/plugins/treesitter')
require('user/plugins/tasks')
require('user/plugins/mason')
