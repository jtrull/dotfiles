return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "folke/neodev.nvim",
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/nvim-cmp",
      "hrsh7th/cmp-nvim-lsp"
    },
    ft = {
      "json", "lua", "ruby", "javascript", "prisma", "typescript",
      "terraform", "yaml"
    },
    config = function()
      require("neodev").setup()
      require("mason").setup()
      require("mason-lspconfig").setup({
        ensure_installed = {
          "jsonls", "lua_ls", "prismals", "ruby_lsp", "ts_ls", "terraformls",
          "yamlls", "eslint"
        }
      })

      local lspconfig = require("lspconfig")
      local capabilities = require("cmp_nvim_lsp").default_capabilities()
      lspconfig.lua_ls.setup {
        capabilities = capabilities,
        settings = { Lua = { diagnostics = { globals = { "vim" } } } }
      }
      lspconfig.prismals.setup { capabilities = capabilities }
      lspconfig.ruby_lsp.setup { capabilities = capabilities }
      lspconfig.ts_ls.setup { capabilities = capabilities }
      lspconfig.terraformls.setup { capabilities = capabilities }
      lspconfig.yamlls.setup { capabilities = capabilities }
      lspconfig.eslint.setup { capabilities = capabilities }
    end
  },
  {
    "kosayoda/nvim-lightbulb",
    ft = {
      "json", "lua", "ruby", "javascript", "prisma", "typescript",
      "terraform", "yaml"
    },
    opts = {
      autocmd = {
        enabled = true,
        updatetime = -1 -- don't mess with updatetime
      }
    }
  }
}

