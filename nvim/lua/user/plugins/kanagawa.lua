return {
  'rebelot/kanagawa.nvim',
  lazy = false,
  opts = {
    colors = {
      theme = {
        all = {
          ui = {
            bg_gutter = "none"
          }
        }
      }
    },
    overrides = function(colors)
      local theme = colors.theme
      return {
        Pmenu = { fg = theme.ui.shade0, bg = theme.ui.bg_p1 },
        PmenuSel = { fg = "NONE", bg = theme.ui.bg_p2 },
        PmenuSbar = { bg = theme.ui.bg_m1 },
        PmenuThumb = { bg = theme.ui.bg_p2 },
      }
    end,
  },
  keys = {
    {
      '<leader>kw',
      '<cmd>colorscheme kanagawa-wave<CR>',
      desc = 'Set Kanagawa Wave colorscheme'
    },
    {
      '<leader>kl',
      '<cmd>colorscheme kanagawa-lotus<CR>',
      desc = 'Set Kanagawa Lotus colorscheme'
    },
    {
      '<leader>kd',
      '<cmd>colorscheme kanagawa-dragon<CR>',
      desc = 'Set Kanagawa Dragon colorscheme'
    },
  },
}
