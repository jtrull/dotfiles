vim.keymap.set({ 'n', 'i', 'v', 'o' }, '<Up>', '<Nop>')
vim.keymap.set({ 'n', 'i', 'v', 'o' }, '<Down>', '<Nop>')
vim.keymap.set({ 'n', 'i', 'v', 'o' }, '<Left>', '<Nop>')
vim.keymap.set({ 'n', 'i', 'v', 'o' }, '<Right>', '<Nop>')

-- Diagnostics
vim.keymap.set('n', '<leader>dd', vim.diagnostic.open_float)
vim.keymap.set('n', '<leader>dq', vim.diagnostic.setloclist)

vim.keymap.set('n', '<leader>\\', '<cmd>nohl<cr>')

