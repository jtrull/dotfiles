-- Autocommands to handle plugin updates
vim.api.nvim_create_autocmd("PackChanged", {
  pattern = "*",
  callback = function(ev)
    local name, kind = ev.data.spec.name, ev.data.kind
    if not (name == "nvim-treesitter" and kind == "update") then return end
    if not ev.data.active then vim.cmd.packadd("nvim-treesitter") end
    vim.cmd("TSUpdate")
  end
})

vim.pack.add({
  "https://github.com/nvim-treesitter/nvim-treesitter",
  "https://github.com/neovim/nvim-lspconfig",
  "https://github.com/mason-org/mason.nvim",
  "https://github.com/mason-org/mason-lspconfig.nvim",
  "https://github.com/kosayoda/nvim-lightbulb"
})

local treesitter_languages = {
  "bash", "css", "csv", "diff", "dockerfile",
  "git_config", "git_rebase", "gitcommit", "gitignore",
  "graphql", "html", "http", "java", "javascript", "jq",
  "json", "json5",
  "lua", "luadoc", "make", "perl", "prisma",
  "python", "regex", "requirements",
  "ruby", "scss", "sql", "ssh_config", "terraform",
  "toml", "tsv",
  "typescript", "vim", "vimdoc", "xml", "yaml"
}
require("nvim-treesitter").install(treesitter_languages)

local treesitter_filetypes = vim.iter(treesitter_languages):map(vim.treesitter.language.get_filetypes):flatten():totable()
local ts_start = function()
  vim.treesitter.start()
  vim.wo[0][0].foldexpr = "v:lua.vim.treesitter.foldexpr()"
  vim.wo[0][0].foldmethod = "expr"
  vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
end
vim.api.nvim_create_autocmd("FileType", { pattern = treesitter_filetypes, callback = ts_start })

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
