local api = vim.api
local M = {}

function M.makeScratch()
	api.nvim_command('enew')
	vim.bo[0].buftype=nofile
	vim.bo[0].bufhidden=hide
	vim.bo[0].swapfile=false
end

function M.insthm()
  blockType = vim.fn.input("Block type: ")
  name = vim.fn.input("Title: ")
  className = blockType:lower()
  cid = string.gsub(name:lower(), "[^%a%d%s-]", "")
  cid = string.gsub(cid, "%s", "-")
  curline_idx = vim.fn.line(".")
  api.nvim_buf_set_lines(0, curline_idx, curline_idx, false,
  {"::: {." .. className .. " name=\"" .. name .. "\" id=" .. cid .. "}",
  "",
  ":::"})
  vim.fn.cursor(curline_idx + 2, 0)
end

function M.insproof()
  curline_idx = vim.fn.line(".")
  api.nvim_buf_set_lines(0, curline_idx, curline_idx, false,
  {"::: {.proof}",
  "",
  ":::"})
  vim.fn.cursor(curline_idx + 2, 0)
end

function M.reloadCurrent()
  local fname = api.nvim_buf_get_name(0)
  local module = string.find(fname, "(.+).lua$")
  require('plenary.reload').reload_module(module)
end

return M
