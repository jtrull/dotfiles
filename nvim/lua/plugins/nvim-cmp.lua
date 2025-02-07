return {
  "hrsh7th/nvim-cmp",
  version = false,
  event = { "InsertEnter", "CmdlineEnter" },
  dependencies = {
--    "neovim/nvim-lspconfig",
    "hrsh7th/cmp-nvim-lsp",
    "hrsh7th/cmp-buffer",
    "hrsh7th/cmp-path",
    "hrsh7th/cmp-cmdline",
    "zbirenbaum/copilot-cmp",
    "onsails/lspkind.nvim"
  },
  config = function()
    local cmp = require('cmp')
    cmp.setup({
      mapping = cmp.mapping.preset.insert({
        ['<C-b>'] = cmp.mapping.scroll_docs(-4),
        ['<C-f>'] = cmp.mapping.scroll_docs(4),
        ['<C-Space>'] = cmp.mapping.complete(),
        ['<C-e>'] = cmp.mapping.abort(),
        ['<TAB>'] = cmp.mapping.confirm({ select = true })
      }),
      sources = cmp.config.sources({
        { name = "copilot" },
        { name = "nvim_lsp" }
      }),
      formatting = {
        format = require("lspkind").cmp_format({
          mode = 'symbol',
          maxwidth = {
            menu = 50, -- leading text (labelDetails)
            abbr = 50, -- actual suggestion item
          },
          ellipsis_char = '...',
          show_labelDetails = true,
          symbol_map = { Copilot = "" }
        })
      }
    })
    cmp.setup.cmdline({ '/', '?' }, {
      mapping = cmp.mapping.preset.cmdline(),
      sources = {
        { name = "buffer" }
      }
    })
    cmp.setup.cmdline(':', {
      mapping = cmp.mapping.preset.cmdline(),
      sources = cmp.config.sources({
        { name = "path" }
      }, {
        { name = "cmdline" }
      })
    })
  end
}
