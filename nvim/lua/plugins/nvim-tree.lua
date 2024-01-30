return {
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
}
