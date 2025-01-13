return {
  {
    "nvim-treesitter/nvim-treesitter",
    lazy = false,
    build = ":TSUpdate",
    config = function ()
      local configs = require("nvim-treesitter.configs")

      configs.setup({
        ensure_installed = {
          "bash", "css", "csv", "diff", "dockerfile",
          "git_config", "git_rebase", "gitcommit", "gitignore",
          "graphql", "html", "http", "java", "javascript", "jq",
          "json", "json5", "jsonc",
          "lua", "luadoc", "make", "perl",
          "ruby", "scss", "sql", "ssh_config", "terraform",
          "toml", "tsv",
          "typescript", "vim", "vimdoc", "xml", "yaml"
        },
        sync_install = false,
        highlight = { enable = true },
        indent = {
          enable = true,
          disable = { "ruby" }
        }
      })
    end
  }
}
