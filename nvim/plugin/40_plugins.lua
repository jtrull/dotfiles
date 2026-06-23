-- Create stub commands that defer plugin setup to first use
local function defer_setup(cmds, setup_fn)
  for _, cmd in ipairs(cmds) do
    vim.api.nvim_create_user_command(cmd, function(opts)
      for _, c in ipairs(cmds) do vim.api.nvim_del_user_command(c) end
      setup_fn()
      vim.cmd(cmd .. " " .. (opts.args or ""))
    end, { nargs = "?" })
  end
end

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
  "https://github.com/nvim-lualine/lualine.nvim",
  { src = "https://github.com/nvim-telescope/telescope.nvim", version = "v0.2.2" },
  "https://github.com/nvim-telescope/telescope-fzf-native.nvim",
  "https://github.com/nvim-treesitter/nvim-treesitter",
  "https://github.com/neovim/nvim-lspconfig",
  "https://github.com/mason-org/mason.nvim",
  "https://github.com/kosayoda/nvim-lightbulb",
  "https://github.com/christoomey/vim-tmux-navigator",
  "https://github.com/moll/vim-bbye",
  "https://github.com/tpope/vim-fugitive",
  "https://github.com/lewis6991/gitsigns.nvim",
  "https://github.com/sindrets/diffview.nvim",
  "https://github.com/Wansmer/treesj",
  "https://github.com/windwp/nvim-autopairs",
  "https://github.com/RRethy/nvim-treesitter-endwise",
  "https://github.com/windwp/nvim-ts-autotag",
  "https://github.com/kylechui/nvim-surround",
  "https://github.com/Vigemus/iron.nvim",
  "https://github.com/folke/trouble.nvim",
  { src = "https://github.com/Saghen/blink.cmp", version = vim.version.range("1.x") },
  "https://github.com/fang2hou/blink-copilot",
  "https://github.com/folke/lazydev.nvim"
})

vim.cmd([[colorscheme dracula]])

local treesitter_languages = {
  "bash", "css", "csv", "diff", "dockerfile",
  "git_config", "git_rebase", "gitcommit", "gitignore",
  "graphql", "hcl", "html", "http", "java", "javascript", "jq",
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

-- mason is loaded on demand (see 50_lsp.lua for PATH + vim.lsp.enable).
-- Servers are installed via the :Mason UI; the deferred setup runs on first use.
defer_setup({ "Mason", "MasonInstall", "MasonInstallAll", "MasonUninstall",
              "MasonUninstallAll", "MasonLog", "MasonUpdate" }, function()
  require("mason").setup()
end)

require("nvim-lightbulb").setup {
  autocmd = {
    enabled = true,
    updatetime = -1 -- don't mess with updatetime
  }
}

require("fidget").setup {}

require("lualine").setup {
  options = {
    section_separators = '',
    component_separators = ''
  },
  sections = {
    lualine_c = { { 'filename', path = 1 } },
    lualine_x = {
      {
        function()
          local clients = vim.lsp.get_clients({ name = "copilot" })
          if #clients > 0 then return " " end
          return ""
        end,
      },
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
        ["dd"] = function(bufnr) require("telescope.actions").delete_buffer(bufnr) end
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

require("gitsigns").setup {
  on_attach = function(bufnr)
    local gitsigns = require('gitsigns')

    local function map(mode, l, r, opts)
      opts = opts or {}
      opts.buffer = bufnr
      vim.keymap.set(mode, l, r, opts)
    end

    -- Navigation
    map('n', ']c', function()
      if vim.wo.diff then
        vim.cmd.normal({']c', bang = true})
      else
        gitsigns.nav_hunk('next')
      end
    end)

    map('n', '[c', function()
      if vim.wo.diff then
        vim.cmd.normal({'[c', bang = true})
      else
        gitsigns.nav_hunk('prev')
      end
    end)

    -- Actions
    map('n', '<leader>hs', gitsigns.stage_hunk)
    map('n', '<leader>hr', gitsigns.reset_hunk)
    map('v', '<leader>hs', function() gitsigns.stage_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
    map('v', '<leader>hr', function() gitsigns.reset_hunk {vim.fn.line('.'), vim.fn.line('v')} end)
    map('n', '<leader>hS', gitsigns.stage_buffer)
    map('n', '<leader>hR', gitsigns.reset_buffer)
    map('n', '<leader>hp', gitsigns.preview_hunk)
    map('n', '<leader>hP', gitsigns.preview_hunk_inline)
    map('n', '<leader>hb', function() gitsigns.blame_line{full=true} end)
    map('n', '<leader>hB', gitsigns.toggle_current_line_blame)
    map('n', '<leader>hd', gitsigns.diffthis)
    map('n', '<leader>hD', function() gitsigns.diffthis('~') end)

    -- Text object
    map({'o', 'x'}, 'ih', ':<C-U>Gitsigns select_hunk<CR>')
  end
}

defer_setup({ "TSJJoin", "TSJSplit", "TSJToggle" }, function()
  require("treesj").setup { use_default_keymaps = false, max_join_length = 120 }
end)

require("nvim-autopairs").setup {
  check_ts = true
}

require("nvim-ts-autotag").setup()

require("nvim-surround").setup()

defer_setup({ "IronRepl", "IronRestart", "IronFocus", "IronHide" }, function()
  require("iron.core").setup({
    config = {
      highlight_last = "IronLastSent",
      scratch_repl = true,
      repl_definition = {
        sh = {
          command = {"zsh"}
        }
      },
      repl_open_cmd = require("iron.view").split.vertical.rightbelow("50%")
    },
    keymaps = {
      send_motion = "<localleader>sc",
      visual_send = "<localleader>sc",
      send_file = "<localleader>sf",
      send_line = "<localleader>sl",
      send_paragraph = "<localleader>sp",
      send_until_cursor = "<localleader>su",
      send_mark = "<localleader>sm",
      mark_motion = "<localleader>mc",
      mark_visual = "<localleader>mc",
      remove_mark = "<localleader>md",
      cr = "<localleader>s<cr>",
      interrupt = "<localleader>s<space>",
      exit = "<localleader>sq",
      clear = "<localleader>cl"
    }
  })
end)

require("trouble").setup()

require("lazydev").setup({
  library = {
    { path = "${3rd}/luv/library", words = { "vim%.uv" } },
  }
})

require("blink.cmp").setup({
  keymap = {
    preset = 'none',
    ['<C-space>'] = { 'show' },
    ['<C-e>'] = { 'cancel' },
    ['<Tab>'] = { 'select_and_accept', 'fallback' },
    ['<C-n>'] = { 'select_next', 'fallback' },
    ['<C-p>'] = { 'select_prev', 'fallback' },
    ['<Down>'] = { 'select_next', 'fallback' },
    ['<Up>'] = { 'select_prev', 'fallback' },
    ['<C-b>'] = { 'scroll_documentation_up', 'fallback' },
    ['<C-f>'] = { 'scroll_documentation_down', 'fallback' },
  },
  appearance = { nerd_font_variant = 'mono' },
  completion = { documentation = { auto_show = true } },
  sources = {
    default = { 'lazydev', 'lsp', 'copilot' },
    providers = {
      lazydev = {
        name = "LazyDev",
        module = "lazydev.integrations.blink",
        score_offset = 100,
      },
      copilot = {
        name = "copilot",
        module = "blink-copilot",
        async = true,
      }
    }
  },
  fuzzy = { implementation = "prefer_rust_with_warning" },
  cmdline = {
    keymap = {
      preset = 'inherit',
      ['<Tab>'] = { 'show', 'select_and_accept', 'fallback' },
    },
    completion = {
      menu = { auto_show = true },
    },
    sources = { 'cmdline', 'buffer' },
  },
  term = { enabled = false }
})

