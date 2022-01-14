local autopairs = require("nvim-autopairs")

autopairs.setup({
    check_ts = true,
    enable_check_bracket_line = false,
    fast_wrap = {
        map = "<M-e>",
    },
})

local disabled = false
local enable = function()
    autopairs.enable()
    disabled = false
end
local disable = function()
    autopairs.disable()
    disabled = true
end

global.toggle_autopairs = function()
    if disabled then
        enable()
        return
    end

    disable()
    vim.cmd("autocmd InsertLeave * ++once lua global.reset_autopairs()")
end

global.reset_autopairs = function()
    if disabled then
        enable()
    end
end
