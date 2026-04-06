return {
  {
    -- Not the recommended way to install this, but necessary while its
    -- luarocks dependencies are messed up.
    -- See https://github.com/rest-nvim/rest.nvim/issues/559
    "rest-nvim/rest.nvim",
    ft = "http",
    build = false,
    dependencies = {
      "nvim-neotest/nvim-nio",
      {
        -- Lazy.nvim does not recognize this library's rocksfile, so add it
        -- to package path manually.
        "manoelcampos/xml2lua",
        config = function(plugin)
          package.path = package.path .. ";" .. plugin.dir .. "/?.lua"
        end,
      },
      "lunarmodules/lua-mimetypes"
    },
  },
}
