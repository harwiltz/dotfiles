local map = vim.api.nvim_set_keymap

map('n', '<leader>q', ':source %<CR>', {})

map('i', '<C-j>', '<cmd>lua return require\'snippets\'.expand_or_advance(1)<CR>', {})

map('n', '<Leader>f', [[<Cmd>lua require('telescope.builtin').find_files({hidden=true, layout_config={prompt_position="top"}})<CR>]], {silent=true})

map('n', '<Leader>t', [[<Cmd>lua tools.insthm()<CR>]], {})
map('n', '<Leader>p', [[<Cmd>lua tools.insproof()<CR>]], {})
