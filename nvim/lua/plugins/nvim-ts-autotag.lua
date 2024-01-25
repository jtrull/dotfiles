return {
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
}
