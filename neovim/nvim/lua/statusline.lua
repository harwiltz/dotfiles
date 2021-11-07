local M = {}

local mode_map = {
  ['n']  = 'normal ',
  ['no'] = 'n:operator pending ',
  ['v']  = 'visual ',
  ['V']  = 'visual:line ',
  ['']   = 'visual:block ',
  ['s']  = 'select ',
  ['S']  = 'select:line ',
  ['']   = 'select:block ',
  ['i']  = 'insert ',
  ['R']  = 'replace ',
  ['Rv'] = 'visual:replace ',
  ['c']  = 'command ',
  ['cv'] = 'vim ex ',
  ['ce'] = 'ex ',
  ['r']  = 'prompt ',
  ['rm'] = 'more ',
  ['r?'] = 'confirm ',
  ['!']  = 'shell ',
  ['t']  = 'terminal'
}

local function mode()
  local m = vim.api.nvim_get_mode().mode
  if mode_map[m] == nil then return m end
  return mode_map[m]
end

vim.api.nvim_exec(
[[
  hi PrimaryBlock   ctermfg=06    ctermbg=00
  hi SecondaryBlock ctermfg=08    ctermbg=00
  hi Blanks         ctermfg=07    ctermbg=00
]], false)

function M.statusline()
  local stl = {
    '%#PrimaryBlock#',
    mode(),
    '%#SecondaryBlock#',
    '%#Blanks#',
    '%f',
    '%m',
    '%=',
    '%#SecondaryBlock#',
    '%l,%c ',
    '%#PrimaryBlock#',
    '%{&filetype}',
  }

  return table.concat(stl)
end

return M
