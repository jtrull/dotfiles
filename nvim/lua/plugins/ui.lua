return {
  {
    "dracula/vim",
    lazy = false,
    priority = 1000,
    config = function()
      vim.cmd([[colorscheme dracula]])
    end
  },
  {
    'nvim-lualine/lualine.nvim',
    dependencies = { 'nvim-tree/nvim-web-devicons' },
    lazy = false,
    config = function()
      local csection = { { 'filename', path = 1 } }
      require('lualine').setup {
        options = {
          section_separators = '',
          component_separators = ''
        },
        sections = {
          lualine_c = csection,
          lualine_x = {
            'encoding',
            { 'fileformat', icons_enabled = false },
            'filetype'
          }
        },
        inactive_sections = {
          lualine_c = csection
        },
        extensions = { 'fugitive', 'man', 'mason', 'nvim-tree', 'quickfix' }
      }
    end
  },
  {
    "nvim-tree/nvim-tree.lua",
    dependencies = { "nvim-tree/nvim-web-devicons" },
    init = function()
      -- Disable netrw in favor of nvim-tree
      vim.g.loaded_netrw = 1
      vim.g.loaded_netrwPlugin = 1
    end,
    opts = {
      update_focused_file = {
        enable = true
      }
    },
    config = true,
    keys = {
      { "<leader>e", "<cmd>NvimTreeFocus<cr>", desc = "Focus file explorer" }
    }
  },
  {
    "christoomey/vim-tmux-navigator",
    lazy = false,
    config = function()
      vim.g.tmux_navigator_save_on_switch = 2
    end
  }
}
