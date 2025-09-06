vim.pack.add({ { src = "https://github.com/nvim-treesitter/nvim-treesitter", version = 'main' } })

require('nvim-treesitter').install({ 'c', 'cpp', 'lua', 'python', 'vimdoc', 'vim', 'bash', 'glsl', 'markdown',
  'markdown_inline', 'yaml', 'gdscript', 'rust' }):wait(300000)
vim.api.nvim_create_autocmd('FileType', {
  pattern = { '<filetype>' },
  callback = function() vim.treesitter.start() end,
})
