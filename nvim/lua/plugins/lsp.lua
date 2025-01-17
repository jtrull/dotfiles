return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "folke/neodev.nvim",
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/nvim-cmp"
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
      lspconfig.lua_ls.setup { settings = { Lua = { diagnostics = { globals = { "vim" } } } } }
      lspconfig.prismals.setup {}
      lspconfig.ruby_lsp.setup {}
      lspconfig.ts_ls.setup {}
      lspconfig.terraformls.setup {}
      lspconfig.yamlls.setup {}
      lspconfig.eslint.setup {}

      vim.api.nvim_create_autocmd('LspAttach', {
        group = vim.api.nvim_create_augroup('UserLspConfig', {}),
        callback = function(ev)
          -- Buffer local mappings
          local opts = { buffer = ev.buf }
          vim.keymap.set('n', 'gD', vim.lsp.buf.declaration, opts)
          vim.keymap.set('n', 'gd', vim.lsp.buf.definition, opts)
          vim.keymap.set('n', 'gr', vim.lsp.buf.references, opts)
          vim.keymap.set('n', 'gi', vim.lsp.buf.implementation, opts)
          vim.keymap.set('n', 'K', vim.lsp.buf.hover, opts)
          vim.keymap.set('n', '<localleader>lk', vim.lsp.buf.signature_help, opts)
          vim.keymap.set('n', '<localleader>lr', vim.lsp.buf.rename, opts)
          vim.keymap.set('n', '<localleader>la', vim.lsp.buf.code_action, opts)
          vim.keymap.set('n', '<localleader>lf', function()
            vim.lsp.buf.format { async = true }
          end, opts)
        end
      })
    end
  }
}

