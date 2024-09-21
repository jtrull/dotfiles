return {
  {
    "tpope/vim-fugitive",
    event = { "BufRead", "BufWrite" },
    keys = {
      { "<leader>gg", "<cmd>Git<cr>", desc = "Fugitive git status" },
      { "<leader>gb", "<cmd>Git blame<cr>", desc = "Fugitive git blame" },
      { "<leader>gd", "<cmd>Git diff -- %<cr>", desc = "Fugitive git diff current file" }
    }
  },
  {
    "lewis6991/gitsigns.nvim",
    event = "BufRead",
    opts = {
      on_attach = function(bufnr)
        local gitsigns = require('gitsigns')

        local function map(mode, l, r, opts)
          opts = opts or {}
          opts.buffer = bufnr
          vim.keymap.set(mode, l, r, opts)
        end

        -- Navigation
        map('n', ']c', function()
          if vim.wo.diff then
            vim.cmd.normal({']c', bang = true})
          else
            gitsigns.nav_hunk('next')
          end
        end)

        map('n', '[c', function()
          if vim.wo.diff then
            vim.cmd.normal({'[c', bang = true})
          else
            gitsigns.nav_hunk('prev')
          end
        end)

        -- Actions
        map('n', '<leader>hs', gitsigns.stage_hunk)
        map('n', '<leader>hr', gitsigns.reset_hunk)
        map('v', '<leader>hs', function() gitsigns.stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
        map('v', '<leader>hr', function() gitsigns.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
        map('n', '<leader>hS', gitsigns.stage_buffer)
        map('n', '<leader>hu', gitsigns.undo_stage_hunk)
        map('n', '<leader>hR', gitsigns.reset_buffer)
        map('n', '<leader>hp', gitsigns.preview_hunk)
        map('n', '<leader>hb', function() gitsigns.blame_line{full=true} end)
        map('n', '<leader>tb', gitsigns.toggle_current_line_blame)
        map('n', '<leader>hd', gitsigns.diffthis)
        map('n', '<leader>hD', function() gitsigns.diffthis('~') end)
        map('n', '<leader>td', gitsigns.toggle_deleted)

        -- Text object
        map({'o', 'x'}, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
      end
    }
  },
  {
    "Wansmer/treesj",
    keys = {
      { "<leader>sj", "<cmd>TSJJoin<cr>", desc = "Smart Join" },
      { "<leader>ss", "<cmd>TSJSplit<cr>", desc = "Smart Split" },
      { "<leader>st", "<cmd>TSJToggle<cr>", desc = "Toggle Smart Split/Join" }
    },
    opts = {
      use_default_keymaps = false,
      max_join_length = 120
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
    config = true
  },
  {
    "kylechui/nvim-surround",
    version = "*",
    event = "VeryLazy",
    config = true
  }
}