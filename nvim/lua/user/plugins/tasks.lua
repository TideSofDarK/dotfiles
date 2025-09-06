vim.pack.add({ "https://github.com/nvim-lua/plenary.nvim" })
vim.pack.add({ "https://github.com/Shatur/neovim-tasks" })

local Path = require('plenary.path')

require('tasks').setup({
  default_params = {
    cmake = {
      cmd = 'cmake',
      build_dir = tostring(Path:new('{cwd}', 'build', '{os}-{build_type}')),
      build_type = 'Debug',
      args = {
        configure = { '-D', 'CMAKE_EXPORT_COMPILE_COMMANDS=1', '-G', 'Ninja' },
      },
    },
  },
  save_before_run = true,
  params_file = '.cache/neovim-tasks.json',
  quickfix = {
    pos = 'botright',
    height = 10,
  },
})

vim.keymap.set('n', '<leader>bc', function()
  require('tasks').start('cmake', 'configure', '')
end, { desc = 'Configure CMake project' })

vim.keymap.set('n', '<leader>bt', function()
  require('tasks').set_module_param('cmake', 'target')
end, { desc = 'Set CMake target' })

vim.keymap.set('n', '<leader>bb', function()
  require('tasks').start('cmake', 'build', '')
end, { desc = 'Build CMake target' })

vim.keymap.set('n', '<leader>br', function()
  require('tasks').start('cmake', 'run', '')
end, { desc = 'Run CMake target' })
