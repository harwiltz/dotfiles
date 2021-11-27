-- paq
local fn = vim.fn
local install_path = fn.stdpath('data') .. '/site/pack/paqs/start/paq-nvim'

if fn.empty(fn.glob(installpath)) > 0 then
  fn.system({'git', 'clone', '--depth=1', 'https://github.com/savq/paq-nvim.git', install_path})
end

require "paq" {
  "savq/paq-nvim";
  "neovim/nvim-lspconfig";
  "lervag/vimtex";
  "mhartington/oceanic-next";
  "nvim-lualine/lualine.nvim";
  "rafamadriz/neon";
  "folke/tokyonight.nvim";
  "theniceboy/nvim-deus";
  "novakne/kosmikoa.nvim";
  "shaunsingh/nord.nvim";
  "FrenzyExists/aquarium-vim";
  "EdenEast/nightfox.nvim";
  "kvrohit/substrata.nvim";
  "norcalli/snippets.nvim";
  {"oberblastmeister/neuron.nvim", branch="unstable"};
  "nvim-lua/popup.nvim";
  "nvim-lua/plenary.nvim";
  "nvim-telescope/telescope.nvim";
  "JuliaEditorSupport/julia-vim";
}

require('maps')
require('settings')
require('statusline')
require('tools')
require('my-snippets')
require('my-telescope')

-- statusline stuff
-- stl = require('statusline')
-- vim.o.statusline = '%!luaeval("stl.statusline()")'
require'lualine'.setup {
  options = {
    theme = 'horizon',
    section_separators = '',
    component_separators = ''
  },
  sections = {
    lualine_a = {'mode'},
    lualine_b = {'branch', 'diff'},
    lualine_c = {'filename'},
    lualine_x = {'encoding', 'filetype'},
    lualine_y = {'progress'},
    lualine_z = {'location'}
  }
}

require 'neuron'.setup {
  virtual_titles = true,
  mappings = true,
  run = nil,
  neuron_dir = "~/slipbox/slipbox",
  leader = "gz"
}
