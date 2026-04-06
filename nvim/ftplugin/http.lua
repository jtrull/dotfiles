-- rest.nvim and its luarocks dependencies, loaded only for http files.
-- Install deps: `luarocks --lua-version 5.1 install nvim-nio xml2lua mimetypes`

local h = io.popen("luarocks --lua-version 5.1 path 2>/dev/null")
if h then
  local out = h:read("*a")
  h:close()
  local lr_path = out:match("LUA_PATH='([^']*)'")
  local lr_cpath = out:match("LUA_CPATH='([^']*)'")
  if lr_path then
    package.path = package.path .. ";" .. lr_path
    vim.env.LUA_PATH = package.path
  end
  if lr_cpath then
    package.cpath = package.cpath .. ";" .. lr_cpath
    vim.env.LUA_CPATH = package.cpath
  end
end

vim.pack.add({"https://github.com/rest-nvim/rest.nvim"})

-- rest.nvim's own ftplugin/http.lua won't run automatically since the pack
-- was added after filetype detection. Source it manually.
local rest_ftplugin = vim.api.nvim_get_runtime_file("ftplugin/http.lua", true)
for _, f in ipairs(rest_ftplugin) do
  if f:find("rest.nvim", 1, true) then
    vim.cmd.source(f)
    break
  end
end
