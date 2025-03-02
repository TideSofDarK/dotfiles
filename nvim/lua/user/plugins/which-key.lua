return {
  'folke/which-key.nvim',
  event = 'VimEnter',
  config = function()
    require('which-key').setup({
      icons = { mappings = false, },
    })
    require('which-key').add {
      { "<leader>\\", group = "Common" },
      { "<leader>c",  group = "Code" },
      { "<leader>g",  group = "Git" },
      { "<leader>k",  group = "Kanagawa" },
      { "<leader>r",  group = "Rename" },
      { "<leader>s",  group = "Search" },
      { "<leader>t",  group = "Toggle" },
      { "<leader>W",  group = "Workspace" },
      { "<leader>D",  group = "Workspace" },
      { "<leader>b",  group = "Build" },
    }
  end
}
