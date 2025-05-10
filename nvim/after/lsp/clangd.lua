return {
  on_attach = function(client)
    -- client.server_capabilities.semanticTokensProvider = nil
    vim.keymap.set('n', '<leader>to', function()
      local params = vim.lsp.util.make_text_document_params(0)
      client:request('textDocument/switchSourceHeader', params, function(err, result)
        if err then
          error(tostring(err))
        end
        if not result then
          vim.notify('Corresponding file can’t be determined', vim.log.levels.ERROR, { title = 'LSP Error!' })
          return
        end
        vim.cmd.edit(vim.uri_to_fname(result))
      end)
    end, { desc = 'Toggle source/header (C/C++)' })
    vim.keymap.set('n', '<leader>tp', function()
      local params = vim.lsp.util.make_text_document_params(0)
      client:request('textDocument/switchSourceHeader', params, function(err, result)
        if err then
          error(tostring(err))
        end
        if not result then
          vim.notify('Corresponding file can’t be determined', vim.log.levels.ERROR, { title = 'LSP Error!' })
          return
        end
        vim.api.nvim_command('vsplit' .. ' ' .. vim.uri_to_fname(result))
      end)
    end, { desc = 'Toggle source/header vertical split (C/C++)' })
  end,
}
