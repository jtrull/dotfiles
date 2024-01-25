vim.api.nvim_create_autocmd(
  { "BufLeave", "FocusLost" },
  { command = "silent! wall" }
)
