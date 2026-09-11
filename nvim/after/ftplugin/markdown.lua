-- Soft wrap on word boundaries (lbr is a no-op unless wrap is on).
vim.opt_local.wrap = true
vim.opt_local.linebreak = true
vim.opt_local.breakindent = true -- wrapped lines keep their indent (nested lists)
vim.opt_local.breakindentopt = "min:20,list:-1"
vim.opt_local.formatlistpat = [[^\s*\%(\d\+[\]:.)}\t ]\|[-*+]\)\s\+]]

-- Note: j/k are remapped to gj/gk globally in plugin/20_keymaps.lua.

-- Prose-friendly defaults.
vim.opt_local.spell = true
vim.opt_local.spelllang = "en"
vim.opt_local.conceallevel = 2

-- Keep wrapping purely visual: no auto hard-wrapping.
vim.opt_local.textwidth = 0
vim.opt_local.formatoptions:remove("t")
