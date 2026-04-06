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

-- vim-fugitive
vim.keymap.set('n', '<leader>gg', '<cmd>Git<cr>', { desc = "Fugitive git status" })
vim.keymap.set('n', '<leader>gb', '<cmd>Git blame<cr>', { desc = "Fugitive git blame" })
vim.keymap.set('n', '<leader>gd', '<cmd>Git diff -- %<cr>', { desc = "Fugitive git diff current file" })

-- treesj
vim.keymap.set('n', '<leader>sj', '<cmd>TSJJoin<cr>', { desc = "Smart Join" })
vim.keymap.set('n', '<leader>ss', '<cmd>TSJSplit<cr>', { desc = "Smart Split" })
vim.keymap.set('n', '<leader>st', '<cmd>TSJToggle<cr>', { desc = "Toggle Smart Split/Join" })

-- iron.nvim
vim.keymap.set('n', '<leader>rs', '<cmd>IronRepl<cr>', { desc = "Start REPL" })
vim.keymap.set('n', '<leader>rr', '<cmd>IronRestart<cr>', { desc = "Restart REPL" })
vim.keymap.set('n', '<leader>rf', '<cmd>IronFocus<cr>', { desc = "Focus REPL" })
vim.keymap.set('n', '<leader>rh', '<cmd>IronHide<cr>', { desc = "Hide REPL" })

-- trouble.nvim
vim.keymap.set('n', '<leader>xx', '<cmd>Trouble diagnostics toggle filter.buf=0<cr>', { desc = "Buffer Diagnostics (Trouble)" })
vim.keymap.set('n', '<leader>xX', '<cmd>Trouble diagnostics toggle<cr>', { desc = "Diagnostics (Trouble)" })
vim.keymap.set('n', '<leader>xS', '<cmd>Trouble symbols toggle focus=false<cr>', { desc = "Symbols (Trouble)" })
vim.keymap.set('n', '<leader>xL', '<cmd>Trouble lsp toggle focus=false win.position=right<cr>', { desc = "LSP Definitions / references / ... (Trouble)" })
vim.keymap.set('n', '<leader>xl', '<cmd>Trouble loclist toggle<cr>', { desc = "Location List (Trouble)" })
vim.keymap.set('n', '<leader>xq', '<cmd>Trouble qflist toggle<cr>', { desc = "Quickfix List (Trouble)" })

