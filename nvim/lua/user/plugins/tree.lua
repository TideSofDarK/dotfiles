return {
  'nvim-tree/nvim-tree.lua',
  dependencies = { 'echasnovski/mini.icons' },
  config = function()
    require('nvim-tree').setup({
      filters = {
        dotfiles = false,
        custom = { '^\\.git' },
        exclude = { vim.fn.stdpath 'config' .. '/lua/custom' },
      },
      disable_netrw = true,
      hijack_netrw = true,
      hijack_cursor = true,
      hijack_unnamed_buffer_when_opening = false,
      sync_root_with_cwd = true,
      update_focused_file = {
        enable = true,
        update_root = false,
      },
      view = {
        adaptive_size = false,
        side = 'left',
        width = 30,
        preserve_window_proportions = true,
      },
      git = {
        enable = true,
        ignore = true,
      },
      filesystem_watchers = {
        enable = true,
      },
      actions = {
        open_file = {
          resize_window = true,
        },
      },
      renderer = {
        root_folder_label = function(path)
          return vim.fn.fnamemodify(path, ":t")
        end,
        highlight_git = true,
        highlight_opened_files = 'icon',

        indent_markers = {
          enable = true,
          inline_arrows = true,
          icons = {
            corner = "└",
            edge = "│",
            item = "│",
            bottom = "─",
            none = " ",
          },
        },

        icons = {
          show = {
            file = false,
            folder = false,
            folder_arrow = true,
            git = false,
          },
        },
      },
    })
  end,
  keys = {
    {
      '<leader>tt',
      '<cmd>NvimTreeToggle<CR>',
      desc = 'Toggle nvim-tree'
    },
  }
}
