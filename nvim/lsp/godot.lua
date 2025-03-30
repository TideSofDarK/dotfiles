local port = os.getenv 'GDScript_Port' or '6008'
local cmd = vim.lsp.rpc.connect('127.0.0.1', tonumber(port))

return {
  cmd = cmd,
  root_markers = { 'project.godot', '.git' },
  filetypes = { 'gd', 'gdscript', 'gdscript3' },
}
