return {
  'saghen/blink.cmp',
  version = '*',
  opts = {
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
      default = { 'lsp', 'path', }, -- 'buffer'
    },
    fuzzy = { implementation = "prefer_rust_with_warning" },
    signature = {
      enabled = true,
      window = { show_documentation = false }
    },
  },
  opts_extend = { "sources.default" }
}
