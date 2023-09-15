#!/usr/bin/env zsh

# Rebind ALT-c to CTRL-e
# bindkey -rM emacs '\ec'
# bindkey -rM vicmd '\ec'
# bindkey -rM viins '\ec'

# zle     -N              fzf-cd-widget
# bindkey -M emacs '\C-e' fzf-cd-widget
# bindkey -M vicmd '\C-e' fzf-cd-widget
# bindkey -M viins '\C-e' fzf-cd-widget

source $ZDOTDIR/scripts_fzf.zsh # fzf Scripts
_fzf_comprun() {
    local command=$1
    shift

    case "$command" in
        cd)           find . -type d | fzf --preview 'tree -C {}' "$@";;
        *)            fzf "$@" ;;
    esac
}

_fzf_compgen_path() {
    rg --files --glob "!.git" "$1"
}

_fzf_compgen_dir() {
   fd --type d --hidden --follow --exclude ".git" "$1"
}

_fzf_comprun() {
  local command=$1
  shift

  case "$command" in
    tree)         find . -type d | fzf --preview 'tree -C {}' "$@";;
    *)            fzf "$@" ;;
  esac
}
