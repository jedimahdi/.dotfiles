set $64BITS = 1

set breakpoint pending on
set disassembly-flavor intel
set debuginfod enabled off

set print pretty on
set confirm off
set verbose off
set prompt \033[34m❯ \033[0m

set output-radix 0x10
set input-radix 0x10

# These make gdb never pause in its output
set height 0
set width 0
