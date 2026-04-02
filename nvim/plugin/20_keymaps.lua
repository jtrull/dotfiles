vim.keymap.set({ 'n', 'i', 'v', 'o' }, '<Up>', '<Nop>')
vim.keymap.set({ 'n', 'i', 'v', 'o' }, '<Down>', '<Nop>')
vim.keymap.set({ 'n', 'i', 'v', 'o' }, '<Left>', '<Nop>')
vim.keymap.set({ 'n', 'i', 'v', 'o' }, '<Right>', '<Nop>')

-- Movement
vim.keymap.set({ 'n', 'v', 's', 'o' }, 'j', "v:count ? 'j' : 'gj'", { expr = true, silent = true })
vim.keymap.set({ 'n', 'v', 's', 'o' }, 'k', "v:count ? 'k' : 'gk'", { expr = true, silent = true })

-- Diagnostics
vim.keymap.set('n', '<leader>dd', vim.diagnostic.open_float)
vim.keymap.set('n', '<leader>dq', vim.diagnostic.setloclist)

-- Additional LSP mappings
vim.keymap.set('n', 'grD', vim.lsp.buf.declaration)
vim.keymap.set('n', 'grd', vim.lsp.buf.definition)

vim.keymap.set('n', '<leader>\\', '<cmd>nohl<cr>')

-- vim-bbye
vim.keymap.set('n', '<leader>bd', '<cmd>Bdelete<cr>', { desc = "Delete buffer" })
