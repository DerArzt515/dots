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

  use 'neovim/nvim-lspconfig'
  use "folke/which-key.nvim"

  use {
    'hoob3rt/lualine.nvim',
    requires = {'kyazdani42/nvim-web-devicons', opt = true}
  }
end)



