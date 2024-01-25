return {
  'nvim-orgmode/orgmode',
  dependencies = {
    { 'nvim-treesitter/nvim-treesitter', lazy = true }
  },
  ft = 'org',
  config = function()
    require('orgmode').setup_ts_grammar()

    require('nvim-treesitter.configs').setup({
      highlight = {
        enable = true,
        additional_vim_regex_highlighting = { 'org' }
      },
      ensure_installed = { 'org' }
    })

    require('orgmode').setup({
      org_agenda_files = '~/org/**/*.org',
      org_default_notes_file = '~/org/todo.org',
      org_hide_leading_stars = true,
--      org_indent_mode = 'indent'
    })

    vim.api.nvim_create_autocmd('FileType', {
      pattern = "org",
      callback = function()
        vim.opt.wrap = true
        vim.opt.linebreak = true
      end
    })
  end
}
