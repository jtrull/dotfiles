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

vim.api.nvim_create_autocmd("PackChanged", {
  pattern = "*",
  callback = function(ev)
    local name, kind = ev.data.spec.name, ev.data.kind
    if not (name == "telescope-fzf-native.nvim" and kind ~= "delete") then return end
    if not ev.data.active then vim.cmd.packadd("telescope-fzf-native.nvim") end
    vim.system({ "make" }, { cwd = ev.data.path })
  end
})

vim.pack.add({
  "https://github.com/nvim-lua/plenary.nvim",
  "https://github.com/dracula/vim",
  "https://github.com/j-hui/fidget.nvim",
  "https://github.com/nvim-tree/nvim-web-devicons",
  "https://github.com/nvim-tree/nvim-tree.lua",
  "https://github.com/AndreM222/copilot-lualine",
  "https://github.com/nvim-lualine/lualine.nvim",
  { src = "https://github.com/nvim-telescope/telescope.nvim", version = "v0.2.2" },
  "https://github.com/nvim-telescope/telescope-fzf-native.nvim",
  "https://github.com/nvim-treesitter/nvim-treesitter",
  "https://github.com/neovim/nvim-lspconfig",
  "https://github.com/mason-org/mason.nvim",
  "https://github.com/mason-org/mason-lspconfig.nvim",
  "https://github.com/kosayoda/nvim-lightbulb",
  "https://github.com/christoomey/vim-tmux-navigator",
  "https://github.com/moll/vim-bbye"
})

vim.cmd([[colorscheme dracula]])

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

require("fidget").setup {
  integration = {
    ["nvim-tree"] = { enable = false },
    ["xcodebuild-nvim"] = { enable = false }
  }
}

require("lualine").setup {
  options = {
    section_separators = '',
    component_separators = ''
  },
  sections = {
    lualine_c = { { 'filename', path = 1 } },
    lualine_x = {
      'copilot',
      'encoding',
      { 'fileformat', icons_enabled = false },
      'filetype'
    }
  },
  inactive_sections = {
    lualine_c = { { 'filename', path = 1 } }
  },
  extensions = { 'fugitive', 'man', 'mason', 'nvim-tree', 'quickfix' }
}

require("nvim-tree").setup {
  update_focused_file = {
    enable = true,
    exclude = function(bufEnterArgs)
      return vim.endswith(bufEnterArgs.file, ".git/COMMIT_EDITMSG")
    end
  },
  on_attach = function(bufnr)
    local api = require("nvim-tree.api")
    -- default mappings
    api.map.on_attach.default(bufnr)
    -- custom mappings
    vim.keymap.set("n", "+", "<cmd>NvimTreeResize +5<cr>", { desc = "NvimTree size +5", buffer = bufnr, noremap = true, silent = true, nowait = true })
    vim.keymap.set("n", "_", "<cmd>NvimTreeResize -5<cr>", { desc = "NvimTree size -5", buffer = bufnr, noremap = true, silent = true, nowait = true })
  end
}

require("telescope").setup {
  defaults = {
    mappings = {
      n = {
        ["dd"] = require("telescope.actions").delete_buffer
      }
    }
  }
}
vim.api.nvim_create_autocmd("VimEnter", {
  callback = function()
    -- must happen after plugin activation
    require("telescope").load_extension("fzf")
  end
})
