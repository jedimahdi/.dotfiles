if status --is-interactive
  alias ls 'ls -hN --color=auto --group-directories-first'
  alias exa 'exa --classify'
  alias grep 'grep --color=auto'
  alias cp 'cp -iv'
  alias mv 'mv -iv'

  alias cbuild="cabal build --enable-tests --enable-benchmarks --write-ghc-environment-files=always -O0"
  alias ctest="cabal test --enable-tests --test-show-details=direct -O0"
  alias cbench="cabal bench --enable-benchmarks -O0"
  alias crun="cabal run -O0"
  alias cinstall="cabal install --overwrite-policy=always --install-method=copy"
  alias crepl="cabal repl --build-depends pretty-simple"
  alias cdoc="cabal haddock --enable-documentation"
end
