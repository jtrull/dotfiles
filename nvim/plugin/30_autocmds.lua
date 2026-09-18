vim.api.nvim_create_autocmd(
  { "BufLeave", "FocusLost" },
  { command = "silent! wall", nested = true }
)

vim.api.nvim_create_autocmd("CmdlineChanged", {
  pattern = { ":", "/", "?" },
  callback = function()
    vim.fn.wildtrigger()
  end
})
