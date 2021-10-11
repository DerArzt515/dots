local o = vim.o -- for the global options
local w = vim.wo -- for the window local options
local b = vim.bo -- for the buffer local options

local utils = require('utils')
local plugin_manager = require('plugins')

vim.g.mapleader = ';'

b.autoindent = true
b.expandtab = true
b.softtabstop = 4
b.shiftwidth = 4
b.tabstop = 4
b.smartindent = true

w.number = true
