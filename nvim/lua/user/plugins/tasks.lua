return {
  "Shatur/neovim-tasks",
  lazy = true,
  dependencies = {
    "nvim-lua/plenary.nvim",
  },
  config = function()
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
  end,
  keys = {
    {
      '<leader>bc',
      function()
        require('tasks').start('cmake', 'configure', '')
      end,
      desc = 'CMake: Configure'
    },
    {
      '<leader>br',
      function()
        require('tasks').start('cmake', 'run', '')
      end,
      desc = 'CMake: Run'
    },
    {
      '<leader>bb',
      function()
        require('tasks').start('cmake', 'build', '')
      end,
      desc = 'CMake: Build'
    },
  },
}
