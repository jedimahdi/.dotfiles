if status --is-interactive
  abbr -a -g c 'clear'

  abbr -a -g t tmux
  abbr -a -g tc 'tmux-sessionizer'
  abbr -a -g tls 'tmux list-sessions'
  abbr -a -g td 'tmux detach'
  abbr -a -g tn 'tmux new-session -s'
  abbr -a -g tksv 'tmux kill-server'
  abbr -a -g tkss 'tmux kill-session -t'
  abbr -a -g ta 'tmux attach'

  abbr -a -g g git
  abbr -a -g lg lazygit
  abbr -a -g gc 'git commit'
  abbr -a -g gcc 'git clean'
  abbr -a -g gc! 'git checkout . && git clean -f'
  abbr -a -g gcf 'git commit --fixup='
  abbr -a -g ga 'git add'
  abbr -a -g gac 'git add . && git commit'
  abbr -a -g gap 'git add --patch'
  abbr -a -g gb 'git branch'
  abbr -a -g gr 'git rebase'
  abbr -a -g gri 'git rebase --interactive'
  abbr -a -g gco 'git checkout'
  abbr -a -g gcom 'git checkout main'
  abbr -a -g gcl 'git clone'
  abbr -a -g gm 'git merge'
  abbr -a -g gmt 'git mergetool'
  abbr -a -g gp 'git pull'
  abbr -a -g gP 'git push'
  abbr -a -g gf 'git fetch'
  abbr -a -g gl 'git log'
  abbr -a -g glp 'git log --patch'
  abbr -a -g gs 'git status'
  abbr -a -g gd 'git diff'
  abbr -a -g gst 'git stash'
  abbr -a -g gsp 'git stash pop'
  abbr -a -g gss 'git stash push --patch'

  abbr -a -g p protonvpn-cli
  abbr -a -g pd 'protonvpn-cli disconnect'
end
