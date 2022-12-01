;; extends

(("function" @keyword) (#set! conceal "ƒ"))
(((dot_index_expression) @field (#eq? @field "vim.cmd"     )) (#set! conceal ""))
