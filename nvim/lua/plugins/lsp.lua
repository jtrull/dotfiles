-- Ruby diagnostic support until neovim 0.10.0 is released with pull
-- diagnostics support.
local _timers = {}
local function setup_ruby_diagnostics(client, buffer)
  if require("vim.lsp.diagnostic")._enable then
    return
  end

  local diagnostic_handler = function()
    local params = vim.lsp.util.make_text_document_params(buffer)
    client.request("textDocument/diagnostic", { textDocument = params }, function(err, result)
      if err then
        local err_msg = string.format("diagnostics error - %s", vim.inspect(err))
        vim.lsp.log.error(err_msg)
      end
      local diagnostic_items = {}
      if result then
        diagnostic_items = result.items
      end
      vim.lsp.diagnostic.on_publish_diagnostics(
        nil,
        vim.tbl_extend("keep", params, { diagnostics = diagnostic_items }),
        { client_id = client.id }
      )
    end)
  end

  diagnostic_handler() -- request diagnostics on buffer when attaching

  vim.api.nvim_buf_attach(buffer, false, {
    on_lines = function()
      if _timers[buffer] then
        vim.fn.timer_stop(_timers[buffer])
      end
      _timers[buffer] = vim.fn.timer_start(200, diagnostic_handler)
    end,
    on_detach = function()
      if _timers[buffer] then
        vim.fn.timer_stop(_timers[buffer])
      end
    end
  })
end

return {
  {
    "neovim/nvim-lspconfig",
    dependencies = {
      "folke/neodev.nvim",
      "williamboman/mason.nvim",
      "williamboman/mason-lspconfig.nvim",
      "hrsh7th/nvim-cmp"
    },
    ft = { "lua", "ruby", "javascript", "typescript", "terraform", "yaml" },
    config = function()
      require("neodev").setup()
      require("mason").setup()
      require("mason-lspconfig").setup({
        ensure_installed = {
          "lua_ls", "ruby_ls", "tsserver", "terraformls", "yamlls"
        }
      })

      local cmp_caps = require("cmp_nvim_lsp").default_capabilities()
      cmp_caps.textDocument.completion.completionItem.snippetSupport = false

      local lspconfig = require("lspconfig")
      lspconfig.lua_ls.setup { capabilities = cmp_caps }
      lspconfig.ruby_ls.setup {
        capabilities = cmp_caps,
        on_attach = setup_ruby_diagnostics
      }
      lspconfig.tsserver.setup { capabilities = cmp_caps }
      lspconfig.terraformls.setup { capabilities = cmp_caps }
      lspconfig.yamlls.setup { capabilities = cmp_caps }
    end
  }
}

