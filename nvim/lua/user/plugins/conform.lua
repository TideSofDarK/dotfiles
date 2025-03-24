return {
  'stevearc/conform.nvim',
  event = { 'BufWritePre' },
  cmd = { 'ConformInfo' },
  keys = {
    {
      '<leader>cf',
      function()
        require('conform').format { async = true, lsp_format = 'fallback' }
      end,
      desc = 'Format buffer',
    },
  },
  opts = {
    notify_on_error = false,
    format_on_save = false,
  },
}
