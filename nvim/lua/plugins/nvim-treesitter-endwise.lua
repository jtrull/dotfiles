return {
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
}
