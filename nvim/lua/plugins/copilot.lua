return {
  {
    "zbirenbaum/copilot.lua",
    cmd = "Copilot",
    event = "InsertEnter",
    opts = {
      -- disabled in favor of cmp completion
      suggestion = { enabled = false },
      panel = { enabled = false },
      filetypes = {
        org = false
      },
      copilot_node_command = (function ()
        local node_version = vim.trim(vim.fn.system("asdf list nodejs | tr -d ' *' | sort -V | tail -n1"))
        return vim.fn.expand("$HOME") .. '/.asdf/installs/nodejs/' .. node_version .. '/bin/node'
      end)()
    }
  },
  {
    "zbirenbaum/copilot-cmp",
    dependencies = { "zbirenbaum/copilot.lua" },
    config = true
  },
  {
    "olimorris/codecompanion.nvim",
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
