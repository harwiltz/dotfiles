local function texenv(name)
  return '\\begin{' .. name .. '}\n$0\n\\end{' .. name .. '}'
end

local function texenvheader(name, label, labelprefix)
  local label = label or false
  local labelprefix = labelprefix or ''
  local header = '\\begin{' .. name ..  '}'
  if label then
    local headerwithlabel = header .. '\\label{'
    if labelprefix:len() == 0 then
      return headerwithlabel
    else
      return headerwithlabel .. labelprefix .. ':'
    end
  else
    return header
  end
end

require'snippets'.snippets = {
  _global = {
    mc = [[\\mathcal{]]
  },
  tex = {
    Al = texenvheader('align'),
    al = texenvheader('align*'),
    ald = texenvheader('aligned'),
    Eq = texenvheader('equation', true, 'eq'),
    fig = texenvheader('figure', label, 'fig'),
    prob = '\\begin{problem}{$0}\n\\end{problem}'
  },
  markdown = {
    Al = texenv('align'),
    al = texenv('align*'),
    ald = texenv('aligned'),
    Eq = texenv('equation')
  }
}
