-- mason's bin dir must be on PATH so vim.lsp.enable() can find the server
-- executables, since we no longer call mason.setup() at startup.
vim.env.PATH = vim.fn.stdpath("data") .. "/mason/bin:" .. vim.env.PATH

-- Activate LSP servers (configs come from nvim-lspconfig's lsp/*.lua, with
-- our after/lsp/*.lua overrides merged on top). Add a server here after
-- installing it via :Mason.
vim.lsp.enable({
  "jsonls", "lua_ls", "prismals", "pyright", "ruff",
  "ruby_lsp", "ts_ls", "terraformls", "yamlls", "eslint",
})

local autoformat_filetypes = { terraform = true }

vim.api.nvim_create_autocmd("LspAttach", {
  callback = function(ev)
    local client = vim.lsp.get_client_by_id(ev.data.client_id)
    if not client then return end

    if autoformat_filetypes[vim.bo[ev.buf].filetype]
       and client:supports_method("textDocument/formatting") then
      vim.api.nvim_create_autocmd("BufWritePre", {
        buffer = ev.buf,
        callback = function()
          vim.lsp.buf.format({ bufnr = ev.buf, id = client.id })
        end,
      })
    end

    if client:supports_method("textDocument/completion") then
      vim.lsp.completion.enable(true, client.id, ev.buf)
      vim.bo[ev.buf].autocomplete = true
      vim.bo[ev.buf].complete = "o"

      vim.keymap.set("i", "<C-Space>", vim.lsp.completion.get, {
        buffer = ev.buf,
        desc = "LSP completion"
      })

      vim.keymap.set("i", "<Tab>", function()
        return vim.fn.pumvisible() == 1 and "<C-n><C-y>" or "<Tab>"
      end, { buffer = ev.buf, expr = true })
    end

    local map = function(lhs, rhs, desc)
      vim.keymap.set("n", lhs, rhs, { buffer = ev.buf, desc = desc })
    end
    map("<localleader>lf", vim.lsp.buf.format, "LSP format")
    map("<localleader>lr", vim.lsp.buf.rename, "LSP rename")
    map("<localleader>la", vim.lsp.buf.code_action, "LSP code action")
  end,
})
