vim.pack.add({ { src = "https://github.com/saghen/blink.cmp", version = "v1.6.0" } })

require('blink.cmp').setup({
  keymap = { preset = 'default' },
  appearance = {
    nerd_font_variant = 'mono'
  },
  completion = {
    menu = {
      draw = {
        components = {
          kind_icon = {
            text = function(ctx)
              return ""
            end,
            -- highlight = function(ctx)
            --   return ""
            -- end,
          },
        }
      }
    },
    documentation = { auto_show = true, treesitter_highlighting = false }
  },
  sources = {
    default = { 'lsp', 'path', },   -- 'buffer'
  },
  fuzzy = { implementation = "lua" },
  signature = {
    enabled = true,
    window = { show_documentation = false }
  },
})
