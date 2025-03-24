return {
  cmd = { 'sourcekit-lsp' },
  root_markers = { 'buildServer.json', '*.xcodeproj', '.wcworkspace', 'compile_commands.json', 'Package.swift', '.git' },
  filetypes = { 'swift', 'objc', 'objcpp', 'c', 'cpp' },
  get_language_id = function(_, ftype)
    local t = { objc = 'objective-c', objcpp = 'objective-cpp' }
    return t[ftype] or ftype
  end,
  capabilities = {
    textDocument = {
      diagnostic = {
        dynamicRegistration = true,
        relatedDocumentSupport = true,
      },
    },
  },
}
