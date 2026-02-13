return {
  {
    "zbirenbaum/copilot.lua",
    cmd = "Copilot",
    event = "InsertEnter",
    config = function()
      local node_version = vim.trim(vim.fn.system("asdf list nodejs | tr -d ' *' | sort -V | tail -n1"))
      require("copilot").setup({
        -- disabled in favor of cmp completion
        suggestion = { enabled = false },
        panel = { enabled = false },
        filetypes = {
          org = false
        },
        copilot_node_command = vim.fn.expand("$HOME") .. '/.asdf/installs/nodejs/' .. node_version .. '/bin/node'
      })
    end
  },
  {
    "zbirenbaum/copilot-cmp",
    dependencies = { "zbirenbaum/copilot.lua" },
    event = "InsertEnter",
    config = true
  },
  {
    "olimorris/codecompanion.nvim",
    cmd = { "CodeCompanion", "CodeCompanionChat", "CodeCompanionActions", "CodeCompanionCmd" },
    dependencies = {
      "zbirenbaum/copilot.lua",
      "nvim-lua/plenary.nvim",
      "nvim-treesitter/nvim-treesitter"
    },
    opts = {
      strategies = {
        chat = {
          adapter = "copilot"
        },
        inline = {
          adapter = "copilot"
        },
        cmd = {
          adapter = "copilot"
        }
      }
    }
  }
}
