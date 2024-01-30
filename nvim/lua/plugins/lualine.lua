return {
  'nvim-lualine/lualine.nvim',
  dependencies = { 'nvim-tree/nvim-web-devicons' },
  lazy = false,
  config = function()
    local csection = { { 'filename', path = 1 } }
    require('lualine').setup {
      options = {
        section_separators = '',
        component_separators = ''
      },
      sections = {
        lualine_c = csection,
        lualine_x = {
          'encoding',
          { 'fileformat', icons_enabled = false },
          'filetype'
        }
      },
      inactive_sections = {
        lualine_c = csection
      },
      extensions = { 'fugitive', 'man', 'mason', 'nvim-tree', 'quickfix' }
    }
  end
}
