vim.pack.add({
  "https://github.com/neovim/nvim-lspconfig",
  "https://github.com/mason-org/mason.nvim",
  "https://github.com/mason-org/mason-lspconfig.nvim",
  "https://github.com/kosayoda/nvim-lightbulb"
})

require("mason").setup()

require("mason-lspconfig").setup {
  ensure_installed = {
    "jsonls", "lua_ls", "prismals", "pyright", "ruff", "ruby_lsp",
    "ts_ls", "terraformls", "yamlls", "eslint"
  }
}

require("nvim-lightbulb").setup {
  autocmd = {
    enabled = true,
    updatetime = -1 -- don't mess with updatetime
  }
}
