vim.pack.add({ "https://github.com/stevearc/conform.nvim" })

require('conform').setup({
  notify_on_error = false,
  format_on_save = false,
})

vim.keymap.set('n', '<leader>cf', function()
  require('conform').format { async = true, lsp_format = 'fallback' }
end, { desc = 'Format buffer' })
