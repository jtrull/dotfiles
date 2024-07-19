return {
  {
    'nvim-orgmode/orgmode',
    dependencies = { "tpope/vim-repeat" },
    ft = 'org',
    config = function()
      require('orgmode').setup({
        org_agenda_files = '~/org/**/*.org',
        org_default_notes_file = '~/org/todo.org',
        org_startup_folded = "content",
        org_startup_indented = true,
        mappings = {
          prefix = "<localleader>"
        }
      })

      vim.api.nvim_create_autocmd('FileType', {
        pattern = "org",
        callback = function()
          vim.opt.wrap = true
          vim.opt.linebreak = true

          vim.keymap.set('i', '<S-CR>', '<cmd>lua require("orgmode").action("org_mappings.meta_return")<CR>', { silent = true, buffer = true })
        end
      })
    end
  },
  {
    'nvim-orgmode/telescope-orgmode.nvim',
    dependencies = {
      "nvim-orgmode/orgmode",
      "nvim-telescope/telescope.nvim"
    },
    keys = {
      { "<localleader>or", '<cmd>lua require("telescope").extensions.orgmode.refile_heading', ft = "org", desc = "Telescope refile orgmode heading" },
      { "<localleader>ol", '<cmd>lua require("telescope").extensions.orgmode.insert_link', ft = "org", desc = "Telescope insert orgmode link" },
      { "<leader>fo", '<cmd>lua require("telescope").extensions.orgmode.search_headings', desc = "Telescope search orgmode headings" },
    },
    config = function()
      require('telescope').load_extension('orgmode')
    end
  },
  {
    'nvim-orgmode/org-bullets.nvim',
    dependencies = { "nvim-orgmode/orgmode" },
    ft = 'org',
    config = true
  }
}
