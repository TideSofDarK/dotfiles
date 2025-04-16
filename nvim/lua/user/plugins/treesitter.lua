return
{
  'nvim-treesitter/nvim-treesitter',
  lazy = false,
  build = ':TSUpdate',
  main = 'nvim-treesitter.configs',
  opts = {
    ensure_installed = { 'c', 'cpp', 'lua', 'python', 'vimdoc', 'vim', 'bash', 'glsl', 'markdown', 'markdown_inline', 'yaml', 'gdscript', 'rust' },
    ignore_install = { 'swift' },
    auto_install = false,
    highlight = {
      enable = true,
    },
    indent = { enable = false },
  },
}
