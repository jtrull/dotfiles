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
      }
    }
  },
  {
    "zbirenbaum/copilot-cmp",
    dependencies = { "zbirenbaum/copilot.lua" },
    config = true
  },
  {
    "CopilotC-Nvim/CopilotChat.nvim",
    dependencies = {
      "zbirenbaum/copilot.lua",
      "nvim-lua/plenary.nvim"
    },
    config = true,
    keys = {
      { "<leader>cc", "<cmd>CopilotChatToggle<cr>", desc = "CopilotChat: Toggle window" },
      { "<leader>ck", "<cmd>CopilotChatStop<cr>", desc = "Copilot Chat: Stop output" },
      { "<leader>cq", "<cmd>CopilotChatReset<cr>", desc = "Copilot Chat: Reset" },
      { "<leader>cx", "<cmd>CopilotChatExplain<cr>", desc = "Copilot Chat: Explain" },
      { "<leader>cr", "<cmd>CopilotChatReview<cr>", desc = "Copilot Chat: Review" },
      { "<leader>cf", "<cmd>CopilotChatFix<cr>", desc = "Copilot Chat: Fix" },
      { "<leader>co", "<cmd>CopilotChatOptimize<cr>", desc = "Copilot Chat: Optimize" },
      { "<leader>cd", "<cmd>CopilotChatDocs<cr>", desc = "Copilot Chat: Generate documentation comments" },
      { "<leader>ct", "<cmd>CopilotChatTests<cr>", desc = "Copilot Chat: Generate tests" },
      { "<leader>cg", "<cmd>CopilotChatCommit<cr>", desc = "Copilot Chat: Generate commit message" }
    }
  }
}
