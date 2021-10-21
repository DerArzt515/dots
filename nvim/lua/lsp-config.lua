local lsp = require('lspconfig')
local cmd = vim.cmd

-- Python
lsp.pyright.setup{
	cmd = { "pyright-langserver", "--stdio" },
	filetypes = { "python" },
        root_dir = function(fname)
            local root_files = {
		    'pyproject.toml',
		    'setup.py',
		    'setup.cfg',
		    'requirements.txt',
		    'Pipfile',
		    'pyrightconfig.json',
            }
            return util.root_pattern(unpack(root_files))(fname) or util.find_git_ancestor(fname) or util.path.dirname(fname)
        end,
	settings = {
		python = {
			analysis = {
				autoSearchPaths = true,
				diagnosticMode = "workspace",
				useLibraryCodeForTypes = true
			}
		}
	}
}

----------------------------------
-- LSP Setup ---------------------
----------------------------------
metals_config = require("metals").bare_config()

-- Example of settings
metals_config.settings = {
  showImplicitArguments = true,
  excludedPackages = { "akka.actor.typed.javadsl", "com.github.swagger.akka.javadsl" },
}

-- Example of how to ovewrite a handler
metals_config.handlers["textDocument/publishDiagnostics"] = vim.lsp.with(vim.lsp.diagnostic.on_publish_diagnostics, {
  virtual_text = { prefix = "" },
})

-- *READ THIS*
-- I *highly* recommend setting statusBarProvider to true, however if you do,
-- you *have* to have a setting to display this in your statusline or else
-- you'll not see any messages from metals. There is more info in the help
-- docs about this
metals_config.init_options.statusBarProvider = "on"

-- Example if you are including snippets
local capabilities = vim.lsp.protocol.make_client_capabilities()
capabilities.textDocument.completion.completionItem.snippetSupport = true

metals_config.capabilities = capabilities

-- Debug settings if you're using nvim-dap
-- local dap = require("dap")

-- dap.configurations.scala = {
--   {
--     type = "scala",
--     request = "launch",
--     name = "Run",
--     metals = {
--       runType = "run",
--       -- again... example, don't leave these in here
--       args = { "firstArg", "secondArg", "thirdArg" },
--     },
--   },
--   {
--     type = "scala",
--     request = "launch",
--     name = "Test File",
--     metals = {
--       runType = "testFile",
--     },
--   },
--   {
--     type = "scala",
--     request = "launch",
--     name = "Test Target",
--     metals = {
--       runType = "testTarget",
--     },
--   },
-- }

-- metals_config.on_attach = function(client, bufnr)
--   require("metals").setup_dap()
-- end

-- Should link to something to see your code lenses
cmd([[hi! link LspCodeLens CursorColumn]])
-- Should link to something so workspace/symbols are highlighted
cmd([[hi! link LspReferenceText CursorColumn]])
cmd([[hi! link LspReferenceRead CursorColumn]])
cmd([[hi! link LspReferenceWrite CursorColumn]])

-- If you want a :Format command this is useful
cmd([[command! Format lua vim.lsp.buf.formatting()]])
cmd [[au FileType scala,sbt lua require("metals").initialize_or_attach({metals_config})]]

