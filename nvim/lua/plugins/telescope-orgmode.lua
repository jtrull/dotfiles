return {
  'joaomsa/telescope-orgmode.nvim',
  dependencies = { "nvim-telescope/telescope.nvim" },
  keys = {
    { "<leader>fo", "<cmd>Telescope orgmode search_headings<cr>", desc = "Telescope search orgmode headings" },
    { "<leader>or", "<cmd>Telescope orgmode refile_heading<cr>", ft = "org", desc = "Telescope refile orgmode heading" }
  },
  config = function()
    require('telescope').load_extension('orgmode')
  end
}

