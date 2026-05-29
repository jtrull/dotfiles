-- Enable the bytecode + module-path cache before any require runs.
-- Persists to ~/.cache/nvim/luac, cutting both warm and cold startup.
vim.loader.enable()
