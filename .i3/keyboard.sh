setxkbmap -layout us,ir
setxkbmap -option 'grp:alt_shift_toggle'
setxkbmap -option caps:escape
setxkbmap -option 'caps:ctrl_modifier' # makes Caps another Ctrl
setxkbmap -option 'shift:both_capslock_cancel' # when both Shift buttons are pressed simultaneously, they turn Caps Lock on. it can be switched off by pressing Shift
xcape -e 'Caps_Lock=Escape' # Caps is Esc when pressed by itself
