require 'user.options'
require 'user.keymaps'

if vim.fn.has('nvim-0.12') == 1 then
  require 'user.plugins'
end
