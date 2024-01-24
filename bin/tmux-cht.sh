#!/usr/bin/env bash
languages=('golang' 'nodejs' 'javascript' 'tmux' 'typescript' 'zsh' 'cpp' 'c' 'lua' 'rust' 'bash' 'haskell' 'css' 'html')
commands=('find' 'man' 'tldr' 'sed' 'awk' 'tr' 'cp' 'ls' 'grep' 'xargs' 'rg' 'ps' 'mv' 'kill' 'lsof' 'less' 'head' 'tail' 'tar' 'cp' 'rm' 'rename' 'jq' 'cat' 'ssh' 'cargo' 'git' 'git-worktree' 'git-status' 'git-commit' 'git-rebase' 'docker' 'docker-compose' 'stow' 'chmod' 'chown' 'make')

selected=$(
  echo "${languages[*]} ${commands[*]}" | tr ' ' '\n' |
    fzf-tmux -p 90% \
      --color 'pointer:10,spinner:92,marker:46,bg+:-1' \
      --prompt "  " \
      --exit-0
)

if [[ -z $selected ]]; then
  exit 0
fi

read -r -p "Enter Query: " query

if [[ ${languages[*]} =~ $selected ]]; then
  query=$(echo "$query" | tr ' ' '+')
  # tmux neww bash -c "echo \"curl cht.sh/$selected/$query/\" & curl cht.sh/$selected/$query & while [ : ]; do sleep 1; done"
  tmux neww bash -c "curl -s cht.sh/$selected/$query | bat --paging always --pager less"
else
  echo "$selected"
  echo "$query"
  tmux neww bash -c "curl -s cht.sh/$selected~$query | bat --paging always --pager less"
fi
