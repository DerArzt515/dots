local which_key = require("which-key")

which_key.setup{
  plugins = {
    marks = true, -- shows a list of your marks on ' and `
    registers = true, -- shows your registers on " in NORMAL or <C-r> in INSERT mode
    spelling = {
      enabled = true, -- enabling this will show WhichKey when pressing z= to select spelling suggestions
      suggestions = 20, -- how many suggestions should be shown in the list?
    },
    -- the presets plugin, adds help for a bunch of default keybindings in Neovim
    -- No actual key bindings are created
    presets = {
      operators = true, -- adds help for operators like d, y, ... and registers them for motion / text object completion
      motions = true, -- adds help for motions
      text_objects = true, -- help for text objects triggered after entering an operator
      windows = true, -- default bindings on <c-w>
      nav = true, -- misc bindings to work with windows
      z = true, -- bindings for folds, spelling and others prefixed with z
      g = true, -- bindings for prefixed with g
    },
  },
  -- add operators that will trigger motion and text object completion
  -- to enable all native operators, set the preset / operators plugin above
  operators = { gc = "Comments" },
  key_labels = {
    -- override the label used to display some keys. It doesn't effect WK in any other way.
    -- For example:
    -- ["<space>"] = "SPC",
    -- ["<cr>"] = "RET",
    -- ["<tab>"] = "TAB",
  },
  icons = {
    breadcrumb = "»", -- symbol used in the command line area that shows your active key combo
    separator = "➜", -- symbol used between a key and it's label
    group = "+", -- symbol prepended to a group
  },
  window = {
    border = "none", -- none, single, double, shadow
    position = "bottom", -- bottom, top
    margin = { 1, 0, 1, 0 }, -- extra window margin [top, right, bottom, left]
    padding = { 2, 2, 2, 2 }, -- extra window padding [top, right, bottom, left]
  },
  layout = {
    height = { min = 4, max = 25 }, -- min and max height of the columns
    width = { min = 20, max = 50 }, -- min and max width of the columns
    spacing = 3, -- spacing between columns
    align = "left", -- align columns left, center or right
  },
  ignore_missing = false, -- enable this to hide mappings for which you didn't specify a label
  hidden = { "<silent>", "<cmd>", "<Cmd>", "<CR>", "call", "lua", "^:", "^ "}, -- hide mapping boilerplate
  show_help = true, -- show help message on the command line when the popup is visible
  triggers = "auto", -- automatically setup triggers
  -- triggers = {"<leader>"} -- or specify a list manually
  triggers_blacklist = {
    -- list of mode / prefixes that should never be hooked by WhichKey
    -- this is mostly relevant for key maps that start with a native binding
    -- most people should not need to change this
    i = { "j", "k" },
    v = { "j", "k" },
  },
}

local mapping = {
        -- buffers
	["<leader>b"] = { name = "buffer"},
	["<leader>bn"] = {":bnext<CR>", "Next Buffer"},
	["<leader>bp"] = {":bprevious<CR>", "Previous Buffer"},
	["<leader>bk"] = {":BufferClose<CR>", "Kill Buffer"},
	["<leader>bb"] = {":Telescope buffers<CR>", "Go To Buffer"},

	-- commentary
	["<leader>/"] = {":Commentary<CR>", "Comment"},

        -- windows
	["<leader>w"] = { name = "window"},
	["<leader>ws"] = {":split<CR>", "Split"},
	["<leader>wv"] = {":vsplit<CR>", "Vertical Split"},
	["<leader>wj"] = {"<C-w>j", "To Window Below"},
	["<leader>wk"] = {"<C-w>k", "To Window Above"},
	["<leader>wh"] = {"<C-w>h", "To Window Left"},
	["<leader>wl"] = {"<C-w>l", "To Window Right"},

        -- Explorer
	["<leader>e"] = {":lua require('telescope.builtin').file_browser{}<CR>", "File Browser"},
        
        -- Search
	["<leader>s"] = { name = "search"},
	["<leader>sc"] = {":set hlsearch!<CR>", "Clear Highlighting"},

        -- Git
	["<leader>g"] = { name = "Git"},
	["<leader>gs"] = {":G status<CR>", "Status"},
	["<leader>ga"] = {name = "Add"},
	["<leader>gaa"] = {":G add --all<CR>", "all"},
        ["<leader>gc"] = {":G commit -m \"", "commit"},
        ["<leader>gp"] = {":G push<CR>", "push"},

	-- find
	["<leader>f"] = { name = "Find"},
	["<leader>ff"] = {":DashboardFindFile<CR>", "Find Files"},
	["<leader>fg"] = {":DashboardFindWord<CR>", "Grep"},
	--["<leader>fb"] = {":Telescope buffers<CR>", "Find Buffer"},
	["<leader>fh"] = {":Telescope help_tags<CR>", "Find help tags"},
	["<leader>fp"] = {":edit ~/.config/nvim/init.lua<CR>:cd ~/.config/nvim/<CR>", "Open Config"},


	["<leader>l"] = {name = "LSP"},
	["<leader>lg"] = {name = "Go To"},
	["<leader>lgd"] = {"<cmd>lua vim.lsp.buf.definition()<CR>", "def"},
	["<leader>lgw"] = {"<cmd>lua vim.lsp.buf.workspace_symbol()<CR>", "workspace symbol"},
	["<leader>lgD"] = {"<cmd>lua vim.lsp.buf.document_symbol()<CR>", "document symbol"},
	["<leader>lgr"] = {"<cmd>lua vim.lsp.buf.references()<CR>", "references"},
	["<leader>lgi"] = {"<cmd>lua vim.lsp.buf.implementation()<CR>", "implmentation"},
	--["<leader>lK"] = {"<cmd>lua vim.lsp.buf.hover()<CR>"},
	["<leader>lr"] = {"<cmd>lua vim.lsp.buf.rename()<CR>", "rename"},
	["<leader>lf"] = {"<cmd>lua vim.lsp.buf.formatting()<CR>", "formatting"},
	["<leader>lc"] = {"<cmd>lua vim.lsp.buf.code_action()<CR>", "code action"},
	["<leader>lw"] = {"<cmd>lua require\"metals\".worksheet_hover()<CR>", "worksheet hover"},
	["<leader>la"] = {"<cmd>lua require\"metals\".open_all_diagnostics()<CR>", "open all diagnostics"},
	["<leader>ld"] = {"<cmd>lua vim.lsp.diagnostic.set_loclist()<CR>", "open buffer diagnostics"},
	-- ["[c"] = {"<cmd>lua vim.lsp.diagnostic.goto_prev { wrap = false }<CR>"},
	--["]c"] = {"<cmd>lua vim.lsp.diagnostic.goto_next { wrap = false }<CR>"},
}

local normal_opts = {
	mode = "n",
	prefix="",
	buffer = nil,
	silent = true,
	noremap = true,
	nowait = false,
}
local visual_opts = {
	mode = "v",
	prefix="",
	buffer = nil,
	silent = true,
	noremap = true,
	nowait = false,
}

which_key.register(mapping, normal_opts)
which_key.register(mapping, visual_opts)

