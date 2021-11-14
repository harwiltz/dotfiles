local function texenv(name)
  return '\\begin{' .. name .. '}\n$0\n\\end{' .. name .. '}'
end

require'snippets'.snippets = {
  _global = {
    mc = [[\\mathcal{]]
  },
  markdown = {
    Al = texenv('align'),
    al = texenv('align*'),
    ald = texenv('aligned'),
    Eq = texenv('equation')
  }
}
