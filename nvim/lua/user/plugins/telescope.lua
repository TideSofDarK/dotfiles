return
{
  'nvim-telescope/telescope.nvim',
  branch = '0.1.x',
  dependencies = {
    'nvim-lua/plenary.nvim',
    {
      'nvim-telescope/telescope-fzf-native.nvim',
      build =
      'cmake -S. -Bbuild -DCMAKE_BUILD_TYPE=Release && cmake --build build --config Release && cmake --install build --prefix build'
    },
  },
  config = function()
    local previewers = require("telescope.previewers")

    local new_maker = function(filepath, bufnr, opts)
      opts = opts or {}
      opts.use_ft_detect = false
      previewers.buffer_previewer_maker(filepath, bufnr, opts)
    end

    require('telescope').setup {
      defaults = {
        borderchars = { "━", "┃", "━", "┃", "┏", "┓", "┛", "┗" },
        buffer_previewer_maker = new_maker,
        mappings = {
          i = {
            ['<C-u>'] = false,
            ['<C-d>'] = false,
          },
        },
      },
    }

    pcall(require('telescope').load_extension, 'fzf')

    local builtin = require 'telescope.builtin'

    vim.keymap.set('n', '<leader>?', builtin.oldfiles, { desc = 'Find recently opened files' })
    vim.keymap.set('n', '<leader><space>', builtin.buffers, { desc = 'Find existing buffers' })
    vim.keymap.set('n', '<leader>/', function()
      builtin.current_buffer_fuzzy_find(require('telescope.themes').get_dropdown {
        results_ts_highlight = false,
        border = {
          prompt = { 1, 1, 1, 1 },
          results = { 1, 1, 1, 1 },
          preview = { 1, 1, 1, 1 },
        },
        borderchars = {
          prompt = {  "━", "┃", "━", "┃", "┏", "┓", "┫", "┣" },
          results = { "━", "┃", "━", "┃", "┣", "┫", "┛", "┗" },
          preview = { "━", "┃", "━", "┃", "┏", "┓", "┛", "┗" },
        },
        previewer = false,
      })
    end, { desc = 'Fuzzily search in current buffer' })
    vim.keymap.set('n', '<leader>s/', function()
      builtin.live_grep {
        grep_open_files = true,
        prompt_title = 'Live grep in open files',
      }
    end, { desc = 'Search in Open Files' })
    vim.keymap.set('n', '<leader>ss', builtin.builtin, { desc = 'Search select telescope' })
    vim.keymap.set('n', '<leader>gf', builtin.git_files, { desc = 'Search git files' })
    vim.keymap.set('n', '<leader>sf', builtin.find_files, { desc = 'Search files' })
    vim.keymap.set('n', '<leader>sh', builtin.help_tags, { desc = 'Search help' })
    vim.keymap.set('n', '<leader>sw', builtin.grep_string, { desc = 'Search current word' })
    vim.keymap.set('n', '<leader>sg', builtin.live_grep, { desc = 'Search by grep' })
    vim.keymap.set('n', '<leader>sd', builtin.diagnostics, { desc = 'Search diagnostics' })
    vim.keymap.set('n', '<leader>sr', builtin.resume, { desc = 'Search resume' })
    vim.keymap.set('n', '<leader>sn', function()
      builtin.find_files { cwd = vim.fn.stdpath 'config' }
    end, { desc = 'Search neovim files' })
  end
}
