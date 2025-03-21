return {
  'WhoIsSethDaniel/mason-tool-installer.nvim',
  dependencies = {
    { 'williamboman/mason.nvim', config = true },
  },
  config = function()
    require('mason').setup()
    require('mason-tool-installer').setup { ensure_installed = { 'clangd', 'cmake-language-server', 'lua-language-server', 'glsl_analyzer' } }
  end,
}
