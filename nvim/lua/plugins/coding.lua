return {
  {
    "numToStr/Comment.nvim",
    config = true,
    lazy = false
  },
  {
    "tpope/vim-fugitive",
    event = { "BufRead", "BufWrite" }
  },
  {
    "lewis6991/gitsigns.nvim",
    event = "BufRead",
    config = true
  },
  {
    "Wansmer/treesj",
    keys = {
      { "J", "n", "<cmd>TSJToggle<cr>", desc = "Join Toggle" }
    },
    opts = {
      use_default_keymaps = false,
      max_join_length = 150
    }
  },
  {
    "windwp/nvim-autopairs",
    event = "InsertEnter",
    opts = {
      check_ts = true
    }
  }, {
    "RRethy/nvim-treesitter-endwise",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    ft = { "ruby", "lua", "vimscript", "bash", "elixir", "fish", "julia" },
    config = function()
      require('nvim-treesitter.configs').setup {
        endwise = {
          enable = true
        }
      }
    end
  }, {
    "windwp/nvim-ts-autotag",
    dependencies = { "nvim-treesitter/nvim-treesitter" },
    ft = { "html", "javascript", "jsx", "markdown", "php", "tsx", "typescript", "vue", "xml" },
    config = function()
      require('nvim-treesitter.configs').setup {
        autotag = {
          enable = true
        }
      }
    end
  },
  {
    "kylechui/nvim-surround",
    version = "*",
    event = "VeryLazy",
    config = true
  }
}