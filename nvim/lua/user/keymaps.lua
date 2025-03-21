vim.keymap.set({ 'n', 'v' }, '<Space>', '<Nop>', { silent = true })

-- Toggle line numbers and sign column

vim.keymap.set('n', '<leader>tn',
    function()
        vim.o.signcolumn = vim.o.signcolumn == "yes" and "no" or "yes"
        vim.o.number = not vim.o.number
        vim.o.relativenumber = not vim.o.relativenumber
    end
    , { desc = 'Toggle line numbers and sign column' })

-- Quickly delete buffer

vim.keymap.set('n', '<leader>d', '<cmd>bprevious <bar> bdelete! #<cr>', { desc = 'Delete buffer' })

-- Quicker writes

vim.api.nvim_create_user_command("W", "w", {})
vim.api.nvim_create_user_command("Wa", "wa", {})
vim.keymap.set('n', '<leader>w', '<cmd>:w<cr>', { desc = 'Write buffer' })
vim.keymap.set('n', '<leader>a', '<cmd>:wa<cr>', { desc = 'Write all buffers' })

-- Quickly close window

vim.keymap.set('n', '<leader>q', '<C-w>q', { desc = 'Quit a window' })

-- Jump to the first non-blank character of the line

vim.keymap.set({ 'n', 'v', 'o' }, 'H', '^')

-- Jump to the end of the line

vim.keymap.set({ 'n', 'v', 'o' }, 'L', '$')

-- Easier exit from builtin terminal

vim.keymap.set('t', '<Esc><Esc>', '<C-\\><C-n>', { desc = 'Exit terminal mode' })

-- Remap for dealing with word wrap

vim.keymap.set('n', 'k', "v:count == 0 ? 'gk' : 'k'", { expr = true, silent = true })
vim.keymap.set('n', 'j', "v:count == 0 ? 'gj' : 'j'", { expr = true, silent = true })

-- Better window navigation

vim.keymap.set('n', '<m-h>', '<C-w>h')
vim.keymap.set('n', '<m-j>', '<C-w>j')
vim.keymap.set('n', '<m-k>', '<C-w>k')
vim.keymap.set('n', '<m-l>', '<C-w>l')

-- Highlight on yank

vim.api.nvim_create_autocmd('TextYankPost', {
    desc = 'Highlight when yanking (copying) text',
    group = vim.api.nvim_create_augroup('highlight-yank', { clear = true }),
    callback = function()
        vim.highlight.on_yank()
    end,
})

-- Select lines and move them up/down

vim.keymap.set('v', 'J', ":m '>+1<CR>gv=gv")
vim.keymap.set('v', 'K', ":m '<-2<CR>gv=gv")

-- Paste non-linewise text above or below current line, see https://stackoverflow.com/a/1346777/6064933

vim.keymap.set("n", "<leader>p", "m`o<ESC>p``", { desc = "Paste below current line" })
vim.keymap.set("n", "<leader>P", "m`O<ESC>p``", { desc = "Paste above current line" })

-- Close location list or quickfix list if they are present, see https://superuser.com/q/355325/736190

vim.keymap.set('n', [[\x]], '<cmd>windo lclose <bar> cclose <cr>', {
    silent = true,
    desc = 'Close qf and location list',
})

-- Continuous visual shifting (does not exit Visual mode)
-- See https://superuser.com/q/310417/736190

vim.keymap.set('x', '<', '<gv')
vim.keymap.set('x', '>', '>gv')

-- Change current working directory locally and print cwd after that,
-- See https://vim.fandom.com/wiki/Set_working_directory_to_the_current_file

vim.keymap.set('n', [[\c]], '<cmd>lcd %:p:h<cr><cmd>pwd<cr>', { desc = 'Change CWD to current file' })

vim.keymap.set('n', [[\;]], ':cd ' .. vim.fn.stdpath 'config' .. '<cr>',
    { desc = 'Change CWD to Neovim config directory' })

-- Change text without putting it into the vim register
-- See https://stackoverflow.com/q/54255/6064933

vim.keymap.set("x", "c", '"_c')
vim.keymap.set("n", "C", '"_C')
vim.keymap.set("n", "cc", '"_cc', { desc = 'Whole line' })

-- Replace visual selection with text in register, but not contaminate the register
-- See https://stackoverflow.com/q/10723700/6064933

vim.keymap.set("x", "p", '"_c<Esc>p')

-- Navigation in the location and quickfix list

vim.keymap.set("n", "[l", "<cmd>lprevious<cr>zv", { silent = true, desc = "Previous location item" })
vim.keymap.set("n", "]l", "<cmd>lnext<cr>zv", { silent = true, desc = "Next location item" })
vim.keymap.set("n", "[L", "<cmd>lfirst<cr>zv", { silent = true, desc = "First location item" })
vim.keymap.set("n", "]L", "<cmd>llast<cr>zv", { silent = true, desc = "Last location item" })
vim.keymap.set("n", "[q", "<cmd>cprevious<cr>zv", { silent = true, desc = "Previous quickfix item" })
vim.keymap.set("n", "]q", "<cmd>cnext<cr>zv", { silent = true, desc = "Next quickfix item" })
vim.keymap.set("n", "[Q", "<cmd>cfirst<cr>zv", { silent = true, desc = "First quickfix item" })
vim.keymap.set("n", "]Q", "<cmd>clast<cr>zv", { silent = true, desc = "Last quickfix item" })

-- Diagnostic keymaps

vim.keymap.set('n', '<leader>cd', vim.diagnostic.open_float, { desc = 'Open diagnostic window' })
