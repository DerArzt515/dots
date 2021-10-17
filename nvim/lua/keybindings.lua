local opts = { noremap = true, silent = true }
local opts_silent= { silent = true }



vim.api.nvim_set_keymap('n', '<Space>', '<NOP>', opts)
vim.g.mapleader = ' '
-- vim.api.nvim_set_keymap('n', '<Leader>h', ':set hlsearch!<CR>', opts)
-- vim.api.nvim_set_keymap('n', '<Leader>/', ':Commentary<CR>', opts)
--vim.api.nvim_set_keymap('v', '<Leader>/', ':\'<,\'>Commentary<CR>', opts)

-- Toggle file explorer
-- vim.api.nvim_set_keymap('n', '<Leader>e', ':NvimTreeToggle<CR>', opts)

-- -- window movement
-- vim.api.nvim_set_keymap('n', '<C-h>', '<C-w>h', opts_silent)
-- vim.api.nvim_set_keymap('n', '<C-j>', '<C-w>j', opts_silent)
-- vim.api.nvim_set_keymap('n', '<C-k>', '<C-w>k', opts_silent)
-- vim.api.nvim_set_keymap('n', '<C-l>', '<C-w>l', opts_silent)

-- terminal window nav
-- vim.api.nvim_set_keymap('t', '<C-h>', '<C-\\><C-N><C-w>h', opts_silent)
-- vim.api.nvim_set_keymap('t', '<C-j>', '<C-\\><C-N><C-w>j', opts_silent)
-- vim.api.nvim_set_keymap('t', '<C-k>', '<C-\\><C-N><C-w>k', opts_silent)
-- vim.api.nvim_set_keymap('t', '<C-l>', '<C-\\><C-N><C-w>l', opts_silent)
vim.api.nvim_set_keymap('t', '<Esc>', '<C-\\><C-n>', opts)

-- better block indenting moving
vim.api.nvim_set_keymap('v', '<', '<gv', opts)
vim.api.nvim_set_keymap('v', '>', '>gv', opts)

-- tab switch buffer
-- vim.api.nvim_set_keymap('n', '<TAB>', ':bnext<CR>', opts)
-- vim.api.nvim_set_keymap('n', '<S-TAB>', ':bprevious<CR>', opts)

-- vim.api.nvim_set_keymap('n', '<Leader>k', ':BufferClose<CR>', opts)
-- vim.api.nvim_set_keymap('n', '<Leader>n', ':BufferPick<CR>', opts)

-- resize buffers
vim.api.nvim_set_keymap('n', '<C-Up>', ':resize -2<CR>', opts_silent)
vim.api.nvim_set_keymap('n', '<C-Down>', ':resize +2<CR>', opts_silent)
vim.api.nvim_set_keymap('n', '<C-Left>', ':vertical resize -2<CR>', opts_silent)
vim.api.nvim_set_keymap('n', '<C-Right>', ':vertical resize +2<CR>', opts_silent)


