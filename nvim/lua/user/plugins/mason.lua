return {
  'mason-org/mason-lspconfig.nvim',
  dependencies = {
    { 'mason-org/mason.nvim' },
    { 'neovim/nvim-lspconfig' },
  },
  config = function()
    vim.lsp.config("*", {
      capabilities = vim.lsp.protocol.make_client_capabilities()
    })
    require('mason').setup()
    require('mason-lspconfig').setup { ensure_installed = { 'clangd', 'cmake', 'lua_ls', 'glsl_analyzer' } }
    vim.api.nvim_create_autocmd("LspAttach", {
      group = vim.api.nvim_create_augroup("UserLspAttach", { clear = true }),
      callback = function(event)
        -- vim.lsp.completion.enable(true, ev.data.client_id, ev.buf, { autotrigger = true })

        local map = function(keys, func, desc, mode)
          mode = mode or 'n'
          vim.keymap.set(mode, keys, func, { buffer = event.buf, desc = desc })
        end

        local client = vim.lsp.get_client_by_id(event.data.client_id)

        local lsp_picker = require('mini.extra').pickers.lsp

        map('gO', function() lsp_picker({ scope = 'document_symbol' }) end, 'Document symbols')
        map('gW', function() lsp_picker({ scope = 'workspace_symbol' }) end, 'Workspace symbols')
        map('grr', function() lsp_picker({ scope = 'references' }) end, 'Go to references')
        map('gri', function() lsp_picker({ scope = 'implementation' }) end, 'Go to implementation')
        map('grn', vim.lsp.buf.rename, 'Rename')
        map('gra', vim.lsp.buf.code_action, 'Code action')
        map('gd', vim.lsp.buf.definition, 'Go to definition')
        map('gD', vim.lsp.buf.declaration, 'Go to declaration')

        if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint, event.buf) then
          map('<leader>th', function()
            vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled { bufnr = event.buf })
          end, 'Toggle inlay hints')
        end
      end,
    })
  end,
}
