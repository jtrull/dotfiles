return {
  {
    "nvim-telescope/telescope.nvim",
    branch = "0.1.x",
    dependencies = {
      "nvim-lua/plenary.nvim",
      "nvim-telescope/telescope-fzf-native.nvim"
    },
    config = function()
      local telescope = require("telescope")
      local actions = require("telescope.actions")
      telescope.setup {
        defaults = {
          mappings = {
            n = {
              ["dd"] = actions.delete_buffer
            }
          }
        }
      }
      telescope.load_extension("fzf")
    end,
    keys = {
      { "<leader>ff", "<cmd>Telescope find_files<cr>", desc = "Telescope find files" },
      { "<leader>fg", "<cmd>Telescope live_grep<cr>", desc = "Telescope live grep" },
      { "<leader>fb", "<cmd>Telescope buffers<cr>", desc = "Telescope find buffers" },
      { "<leader>fh", "<cmd>Telescope help_tags<cr>", desc = "Telescope help tags" },
      { "<leader>fm", "<cmd>Telescope man_pages<cr>", desc = "Telescope man pages" },
      { "<leader><leader>", "<cmd>Telescope resume<cr>", desc = "Telescope resume" },
      { "<leader>lr", "<cmd>Telescope lsp_references<cr>", desc = "Telescope LSP references" },
      { "<leader>li", "<cmd>Telescope lsp_implementations<cr>", desc = "Telescope LSP implementations" },
      { "<leader>ld", "<cmd>Telescope lsp_definitions<cr>", desc = "Telescope LSP definitions" },
      { "<leader>lt", "<cmd>Telescope lsp_type_definitions<cr>", desc = "Telescope LSP type definitions" },
      { "<leader>gL", "<cmd>Telescope git_commits<cr>", desc = "Telescope git commits" },
      { "<leader>gl", "<cmd>Telescope git_bcommits<cr>", desc = "Telescope git commits current file" },
      { "<leader>gc", "<cmd>Telescope git_branches<cr>", desc = "Telescope git branches" },
      { "<leader>gs", "<cmd>Telescope git_status<cr>", desc = "Telescope git status" },
      { "<leader>gS", "<cmd>Telescope git_stash", desc = "Telescope git stashes" }
    }
  },
  {
    "nvim-telescope/telescope-fzf-native.nvim",
    build = "make"
  }
}
