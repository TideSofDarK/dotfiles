return
{
  'nvim-treesitter/nvim-treesitter',
  lazy = false,
  build = ':TSUpdate',
  main = 'nvim-treesitter.configs',
  opts = {
    ensure_installed = { 'c', 'cpp', 'lua', 'python', 'vimdoc', 'vim', 'bash', 'glsl', 'markdown', 'markdown_inline', 'yaml' },
    ignore_install = { 'swift' },
    auto_install = true,
    highlight = {
      enable = true,
    },
    indent = { enable = false },
  },
}
