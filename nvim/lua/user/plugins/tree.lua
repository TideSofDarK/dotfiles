return {
  'nvim-tree/nvim-tree.lua',
  lazy = false,
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
        highlight_opened_files = 'none',
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
          glyphs =
          {
            default = "□",
            modified = "■",
            folder = {
              arrow_closed = "⮞",
              arrow_open = "⮟",
              default = "●︎",
              open = "○︎",
              empty = "●︎",
              empty_open = "○︎",
              symlink = "●︎",
              symlink_open = "○︎",
            }
          },
          show = {
            file = false,
            folder = false,
            folder_arrow = false,
            git = false,
          },
        },
      },
    })
    local function close_nvim_tree()
      require("nvim-tree.api").tree.close()
    end
    vim.api.nvim_create_autocmd({ "VimEnter" }, { callback = close_nvim_tree })
  end,
  keys = {
    {
      '<leader>tt',
      '<cmd>NvimTreeToggle<CR>',
      desc = 'Toggle nvim-tree'
    },
  }
}
