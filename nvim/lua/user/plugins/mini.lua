return {
  'echasnovski/mini.nvim',
  version = false,
  config = function()
    local icons = require('mini.icons')
    icons.setup({ style = "ascii" })

    local win_config = function()
      return {
        -- anchor = 'NW',
        -- height = math.floor(0.618 * vim.o.lines),
        -- width = math.floor(0.618 * vim.o.columns),
        -- row = math.floor(0.5 * (vim.o.lines - height)),
        -- col = math.floor(0.5 * (vim.o.columns - width)),
        -- border = { '┏', '━', '┓', '┃', '┛', '━', '┗', '┃' },

        height = math.floor(0.3 * vim.o.lines),
        width = 9999,
        border = { '┏', '━', '┓', '┃', '┛', '━', '┗', '┃' }
      }
    end

    local pick = require('mini.pick')
    pick.setup({
      options = {
        use_cache = true,
      },
      window = {
        prompt_prefix = ' Query: ',
        config = win_config,
      },
    })

    local function override_pick_highlights()
      local border_color = vim.api.nvim_get_hl(0, { name = "FloatBorder", create = false, link = false }).fg
      local title_color = vim.api.nvim_get_hl(0, { name = "FloatTitle", create = false, link = false }).fg
      vim.api.nvim_set_hl(0, "MiniPickNormal", { link = "Normal" })
      vim.api.nvim_set_hl(0, "MiniPickBorder", { fg = border_color })
      vim.api.nvim_set_hl(0, "MiniPickPrompt", { fg = title_color })
      vim.api.nvim_set_hl(0, "MiniPickBorderText", { fg = title_color })
    end
    -- override_pick_highlights()
    vim.api.nvim_create_autocmd("ColorScheme", {
      pattern = "*",
      callback = override_pick_highlights
    })

    local extra = require('mini.extra')
    extra.setup()

    vim.keymap.set('n', '<leader><space>', function()
      pick.builtin.buffers()
    end, { desc = 'Find existing buffers' })

    vim.keymap.set('n', '<leader>sf', function()
      pick.builtin.files()
    end, { desc = 'Find files' })

    vim.keymap.set('n', '<leader>sg', function()
      pick.builtin.grep_live()
    end, { desc = 'Find by grep' })

    vim.keymap.set('n', '<leader>/', function()
      extra.pickers.buf_lines({ preserve_order = false, scope = "current" }, {})
    end, { desc = 'Search buffer lines' })

    vim.keymap.set('n', '<leader>sh', function()
      pick.builtin.help()
    end, { desc = 'Search help' })

    vim.keymap.set('n', '<leader>sr', function()
      pick.builtin.resume()
    end, { desc = 'Search resume' })

    vim.keymap.set('n', '<leader>sn', function()
      pick.builtin.files({}, { source = { name = "Neovim files", cwd = vim.fn.stdpath 'config' } })
    end, { desc = 'Search Neovim files' })

    vim.keymap.set("n", "<leader>st", function()
      local colorscheme = pick.start({
        source = {
          name = "Colorschemes",
          items = vim.fn.getcompletion("", "color"),
        },
      })
      if colorscheme ~= nil then
        vim.cmd("colorscheme " .. colorscheme)
      end
    end, { desc = "Find colorschemes" })
  end,
}
