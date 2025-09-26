return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "hrsh7th/nvim-cmp",
      "hrsh7th/cmp-nvim-lsp"
    },
    config = function()
      vim.lsp.config('*', {
        capabilities = require('cmp_nvim_lsp').default_capabilities()
      })
      vim.lsp.config('lua_ls', {
        settings = {
          Lua = {
            runtime = {
              version = 'LuaJIT',
              path = vim.split(package.path, ';')
            },
            diagnostics = {
              globals = { 'vim' }
            },
            workspace = {
              library = vim.api.nvim_get_runtime_file("", true)
            },
            telemetry = {
              enable = false
            }
          }
        }
      })
    end
  },
  {
    "mason-org/mason-lspconfig.nvim",
    dependencies = {
      { "mason-org/mason.nvim", opts = {} },
      "neovim/nvim-lspconfig"
    },
    ft = {
      "json", "lua", "ruby", "javascript", "prisma", "typescript",
      "terraform", "yaml"
    },
    opts = {
      ensure_installed = {
        "jsonls", "lua_ls", "prismals", "ruby_lsp", "ts_ls", "terraformls",
        "yamlls", "eslint"
      }
    }
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

