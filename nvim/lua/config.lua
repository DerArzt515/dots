local global = vim.g  -- global variables
local window = vim.w  -- window variables
local tabpage = vim.t -- tabpage variables
local vim_var = vim.v -- predefined vim variables
local env = vim.env   -- environment variables
local setbufferlocal = vim.bo --  equivilant to :set local
local set = vim.o 	-- equivilant to :set
local setglobal = vim.go 
local set_window_local = vim.wo

setbufferlocal.expandtab = true
setbufferlocal.shiftwidth = 2
setbufferlocal.softtabstop = 2
setglobal.clipboard = 'unnamed'

