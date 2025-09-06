vim.pack.add({
  "https://github.com/tpope/vim-fugitive",
  "https://github.com/tpope/vim-sleuth",
  "https://github.com/miikanissi/modus-themes.nvim"
})

vim.cmd [[colorscheme modus_vivendi]]

require('user/plugins/mini')
require('user/plugins/blink')
require('user/plugins/gitsigns')
require('user/plugins/conform')
require('user/plugins/treesitter')
require('user/plugins/tasks')
require('user/plugins/mason')
