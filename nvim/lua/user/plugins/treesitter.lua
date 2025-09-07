vim.pack.add({ { src = "https://github.com/nvim-treesitter/nvim-treesitter", version = 'main' } })

require('nvim-treesitter').install({ 'c', 'cpp', 'lua', 'python', 'vimdoc', 'vim', 'bash', 'glsl', 'markdown',
  'markdown_inline', 'yaml', 'gdscript', 'rust' })
vim.api.nvim_create_autocmd('FileType', {
  callback = function(ctx)
    local has_started = pcall(vim.treesitter.start)
    local no_indent = {}
    if has_started and not vim.list_contains(no_indent, ctx.match) then
      vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
    end
  end,
})
