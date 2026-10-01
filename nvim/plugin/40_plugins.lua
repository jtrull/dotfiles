-- Autocommands to handle package updates
local function on_pack_update(package, fn)
  vim.api.nvim_create_autocmd("PackChanged", {
    pattern = package,
    callback = function (ev)
      if ev.data.kind == "delete" then return end
      if not ev.data.active then vim.cmd.packadd(package) end
      fn(ev)
    end
  })
end

on_pack_update("nvim-treesitter", function () vim.cmd("TSUpdate") end)
on_pack_update("telescope-fzf-native.nvim", function (ev)
  vim.system({ "make" }, { cwd = ev.data.path })
end)

-- Eager loads
vim.pack.add({
  "https://github.com/dracula/vim",
  "https://github.com/nvim-tree/nvim-web-devicons",
  "https://github.com/nvim-lualine/lualine.nvim",
  "https://github.com/nvim-treesitter/nvim-treesitter",
  "https://github.com/neovim/nvim-lspconfig",
  "https://github.com/christoomey/vim-tmux-navigator",
  "https://github.com/moll/vim-bbye",
  "https://github.com/tpope/vim-characterize",
  "https://github.com/tpope/vim-fugitive",
  "https://github.com/lewis6991/gitsigns.nvim",
  "https://github.com/kylechui/nvim-surround",
})

-- Lazy loads
vim.pack.add({
  "https://github.com/nvim-lua/plenary.nvim",
  "https://github.com/nvim-tree/nvim-tree.lua",
  { src = "https://github.com/nvim-telescope/telescope.nvim", version = "v0.2.2" },
  "https://github.com/nvim-telescope/telescope-fzf-native.nvim",
  "https://github.com/mason-org/mason.nvim",
  "https://github.com/kosayoda/nvim-lightbulb",
  "https://github.com/Wansmer/treesj",
  "https://github.com/windwp/nvim-autopairs",
  "https://github.com/RRethy/nvim-treesitter-endwise",
  "https://github.com/windwp/nvim-ts-autotag",
  "https://github.com/Vigemus/iron.nvim",
  "https://github.com/sindrets/diffview.nvim",
  "https://github.com/folke/lazydev.nvim"
}, { load = function () end })

-- Eager setup

-- Color scheme
vim.cmd([[colorscheme dracula]])

-- Treesitter
local treesitter_languages = {
  "bash", "css", "csv", "diff", "dockerfile", "embedded_template",
  "git_config", "git_rebase", "gitcommit", "gitignore",
  "graphql", "hcl", "html", "http", "java", "javascript", "jq",
  "json", "json5",
  "lua", "luadoc", "make", "markdown", "markdown_inline", "perl", "prisma",
  "python", "regex", "requirements",
  "ruby", "scss", "sql", "ssh_config", "terraform",
  "toml", "tsv", "tsx",
  "typescript", "vim", "vimdoc", "xml", "yaml"
}
vim.api.nvim_create_user_command("TSInstallConfigured", function ()
  require("nvim-treesitter").install(treesitter_languages)
end, {
  desc = "Install missing treesitter parsers from the configured list"
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = "*",
  callback = function()
    if not pcall(vim.treesitter.start) then return end
    vim.wo[0][0].foldexpr = "v:lua.vim.treesitter.foldexpr()"
    vim.wo[0][0].foldmethod = "expr"
    vim.bo.indentexpr = "v:lua.require'nvim-treesitter'.indentexpr()"
  end
})

-- Lualine status line
require("lualine").setup {
  options = {
    section_separators = '',
    component_separators = ''
  },
  sections = {
    lualine_c = { { 'filename', path = 1 } },
    lualine_x = {
      'lsp_status',
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

require("nvim-surround").setup()

-- Lazy setups

-- Command stub triggers
local function defer_setup(cmds, setup_fn)
  for _, cmd in ipairs(cmds) do
    vim.api.nvim_create_user_command(cmd, function(opts)
      for _, c in ipairs(cmds) do vim.api.nvim_del_user_command(c) end
      setup_fn()

      local range = opts.range > 0
        and (opts.line1 .. "," .. opts.line2)
        or ""

      vim.cmd(range .. cmd .. " " .. opts.args)
    end, { nargs = "*", range = true })
  end
end

defer_setup({ "Mason", "MasonInstall", "MasonUninstall",
              "MasonUninstallAll", "MasonLog", "MasonUpdate" }, function()
  vim.cmd.packadd("mason.nvim")
  require("mason").setup()
end)

defer_setup({ "DiffviewOpen", "DiffviewFileHistory" }, function ()
  vim.cmd.packadd("diffview.nvim")
end)

defer_setup({ "NvimTreeFocus" }, function ()
  vim.cmd.packadd("nvim-tree.lua")
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
end)

defer_setup({ "Telescope" }, function ()
  vim.cmd.packadd("plenary.nvim")
  vim.cmd.packadd("telescope-fzf-native.nvim")
  vim.cmd.packadd("telescope.nvim")

  local telescope = require("telescope")
  telescope.setup {
    defaults = {
      mappings = {
        n = {
          ["dd"] = function(bufnr) require("telescope.actions").delete_buffer(bufnr) end
        }
      }
    }
  }
  telescope.load_extension("fzf")
end)

defer_setup({ "TSJJoin", "TSJSplit", "TSJToggle" }, function()
  vim.cmd.packadd("treesj")
  require("treesj").setup { use_default_keymaps = false, max_join_length = 120 }
end)

defer_setup({ "IronRepl", "IronRestart", "IronFocus", "IronHide" }, function()
  vim.cmd.packadd("iron.nvim")
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

-- LSP triggers
vim.api.nvim_create_autocmd("LspAttach", {
  once = true,
  callback = function()
    vim.cmd.packadd("nvim-lightbulb")
    require("nvim-lightbulb").setup {
      autocmd = {
        enabled = true,
        updatetime = -1 -- don't mess with updatetime
      }
    }
  end
})

-- Insert mode triggers
vim.api.nvim_create_autocmd("InsertEnter", {
  once = true,
  callback = function ()
    vim.cmd.packadd("nvim-autopairs")
    require("nvim-autopairs").setup {
      check_ts = true
    }
  end
})

-- Filetype triggers
vim.api.nvim_create_autocmd("FileType", {
  pattern = {
    "html", "xml", "eruby", "markdown", "javascript", "javascriptreact",
    "typescript", "typescriptreact"
  },
  callback = function (ev)
    vim.api.nvim_del_autocmd(ev.id)
    vim.cmd.packadd("nvim-ts-autotag")
    require("nvim-ts-autotag").setup()
  end
})

vim.api.nvim_create_autocmd("FileType", {
  pattern = { "ruby", "eruby", "lua" },
  callback = function (ev)
    vim.api.nvim_del_autocmd(ev.id)
    vim.cmd.packadd("nvim-treesitter-endwise")
    require("nvim-treesitter.endwise").attach(ev.buf)
  end
})

vim.api.nvim_create_autocmd("FileType", {
  once = true,
  pattern = { "lua" },
  callback = function ()
    vim.cmd.packadd("lazydev.nvim")
    require("lazydev").setup({
      library = {
        { path = "${3rd}/luv/library", words = { "vim%.uv" } },
      }
    })
  end
})
