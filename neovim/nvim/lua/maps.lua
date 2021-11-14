local map = vim.api.nvim_set_keymap

map('n', '<leader>q', ':source %<CR>', {})

map('i', '<C-k>', '<cmd>lua return require\'snippets\'.expand_or_advance(1)<CR>', {})
