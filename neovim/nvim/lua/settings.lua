local tabwidth = 2

vim.o.incsearch = true
vim.o.expandtab = true
vim.o.shiftwidth = tabwidth
vim.o.tabstop = tabwidth
vim.o.number = true
vim.o.rnu = true -- relativenumber
vim.o.termguicolors = true
vim.o.wildmenu = true
vim.o.wildignore = [[
.git,.svn
*.toc,*.out,*.aux
*.lof,*.blg,*.bbl
*.pyc,*.o,*.class
]]

vim.g.neon_style = "dark"
vim.cmd[[colorscheme nightfox]]
