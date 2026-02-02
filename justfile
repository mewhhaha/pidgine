set shell := ["bash", "-lc"]

bench:
  mkdir -p .cabal .cabal-logs
  CABAL_DIR="$PWD/.cabal" CABAL_LOGDIR=./.cabal-logs cabal bench --benchmark-options='+RTS -N -s -RTS'
