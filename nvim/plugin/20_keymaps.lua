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

-- nvim-tree
vim.keymap.set('n', '<leader>e', '<cmd>NvimTreeFocus<cr>', { desc = "Focus file explorer" })

vim.keymap.set('n', '<leader>ff', '<cmd>Telescope find_files<cr>', { desc = "Telescope find files" })
vim.keymap.set('n', '<leader>fg', '<cmd>Telescope live_grep<cr>', { desc = "Telescope live grep" })
vim.keymap.set('n', '<leader>fb', '<cmd>Telescope buffers<cr>', { desc = "Telescope find buffers" })
vim.keymap.set('n', '<leader>fh', '<cmd>Telescope help_tags<cr>', { desc = "Telescope help tags" })
vim.keymap.set('n', '<leader>fm', '<cmd>Telescope man_pages<cr>', { desc = "Telescope man pages" })
vim.keymap.set('n', '<leader><leader>', '<cmd>Telescope resume<cr>', { desc = "Telescope resume" })
vim.keymap.set('n', '<leader>lr', '<cmd>Telescope lsp_references<cr>', { desc = "Telescope LSP references" })
vim.keymap.set('n', '<leader>li', '<cmd>Telescope lsp_implementations<cr>', { desc = "Telescope LSP implementations" })
vim.keymap.set('n', '<leader>ld', '<cmd>Telescope lsp_definitions<cr>', { desc = "Telescope LSP definitions" })
vim.keymap.set('n', '<leader>lt', '<cmd>Telescope lsp_type_definitions<cr>', { desc = "Telescope LSP type definitions" })
vim.keymap.set('n', '<leader>gL', '<cmd>Telescope git_commits<cr>', { desc = "Telescope git commits" })
vim.keymap.set('n', '<leader>gl', '<cmd>Telescope git_bcommits<cr>', { desc = "Telescope git commits current file" })
vim.keymap.set('n', '<leader>gc', '<cmd>Telescope git_branches<cr>', { desc = "Telescope git branches" })
vim.keymap.set('n', '<leader>gs', '<cmd>Telescope git_status<cr>', { desc = "Telescope git status" })
vim.keymap.set('n', '<leader>gS', '<cmd>Telescope git_stash', { desc = "Telescope git stashes" })

