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

    if client and client.name == 'clangd' then
      -- client.server_capabilities.semanticTokensProvider = nil

      map('<leader>to', function()
        local params = { uri = vim.uri_from_bufnr(event.buf) }
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
      end, 'Toggle source/header (C/C++)')
      map('<leader>tp', function()
        local params = { uri = vim.uri_from_bufnr(event.buf) }
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
      end, 'Toggle source/header vertical split (C/C++)')
    end

    if client and client:supports_method(vim.lsp.protocol.Methods.textDocument_inlayHint, event.buf) then
      map('<leader>th', function()
        vim.lsp.inlay_hint.enable(not vim.lsp.inlay_hint.is_enabled { bufnr = event.buf })
      end, 'Toggle inlay hints')
    end
  end,
})

vim.lsp.enable('clangd')
vim.lsp.enable('cmake-language-server')
vim.lsp.enable('glsl_analyzer')
vim.lsp.enable('lua-language-server')
vim.lsp.enable('sourcekit-lsp')
