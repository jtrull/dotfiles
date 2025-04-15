vim.keymap.set({ 'n', 'i', 'v', 'o' }, '<Up>', '<Nop>')
vim.keymap.set({ 'n', 'i', 'v', 'o' }, '<Down>', '<Nop>')
vim.keymap.set({ 'n', 'i', 'v', 'o' }, '<Left>', '<Nop>')
vim.keymap.set({ 'n', 'i', 'v', 'o' }, '<Right>', '<Nop>')

-- Diagnostics
vim.keymap.set('n', '<leader>dd', vim.diagnostic.open_float)
vim.keymap.set('n', '<leader>dq', vim.diagnostic.setloclist)

-- Additional LSP mappings
vim.keymap.set('n', 'grD', vim.lsp.buf.declaration)
vim.keymap.set('n', 'grd', vim.lsp.buf.definition)

vim.keymap.set('n', '<leader>\\', '<cmd>nohl<cr>')

