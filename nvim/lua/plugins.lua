return require('packer').startup(function()
  use 'wbthomason/packer.nvim'
  use 'tpope/vim-vinegar'
  use 'tpope/vim-fugitive'
  use 'tpope/vim-commentary'

  use 'tomasr/molokai'
  use 'EdenEast/nightfox.nvim'

  use {
    'kyazdani42/nvim-tree.lua',
    requires = 'kyazdani42/nvim-web-devicons',
    config = function() require'nvim-tree'.setup {} end
  }

  use {
	  'romgrk/barbar.nvim',
	  requires = {'kyazdani42/nvim-web-devicons'}
  }
  use {
      'neovim/nvim-lspconfig',
      'williamboman/nvim-lsp-installer',
  }
  use "folke/which-key.nvim"

  use {
    'hoob3rt/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true}
  }


  use 'kristijanhusak/orgmode.nvim'
  use 'akinsho/org-bullets.nvim'
  use 'lukas-reineke/headlines.nvim'
  use {
        'nvim-treesitter/nvim-treesitter',
        run = ':TSUpdate'
  }
  use {
    'nvim-telescope/telescope.nvim',
    requires = { {'nvim-lua/plenary.nvim'} }
  }

  use 'glepnir/dashboard-nvim'

  use 'SirVer/ultisnips'
  use 'honza/vim-snippets'
  use 'neovim/nvim-lspconfig'
  use 'hrsh7th/cmp-nvim-lsp'
  use 'hrsh7th/cmp-buffer'
  use 'hrsh7th/nvim-cmp'
  use 'quangnguyen30192/cmp-nvim-ultisnips'
  use{'scalameta/nvim-metals',
	  requires = {{ "nvim-lua/plenary.nvim" }}
  }
end)



