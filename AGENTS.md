# Benchmark Focus

Current performance target set:

- `program/10k/eachm`
- `program/10k/eachm-aztecs`
- `program/10k+1/eachm`
- `program/10k+1/eachm-aztecs`

Use `cabal bench` directly (no `CABAL_DIR` override).

Time runs:

```sh
cabal bench redatared-bench --ghc-options=-O2 --benchmark-options='--match glob <benchmark-name> +RTS -N -s -RTS'
```

Allocation comparisons:

```sh
cabal bench redatared-bench --ghc-options=-O2 --benchmark-options='--match glob <benchmark-name> --iters 1000 +RTS -N -s -RTS'
```

Notes:

- `+RTS -N -s -RTS` enables parallel runtime and RTS allocation stats.
- For fair allocation per-op comparisons, use fixed iteration count (currently `--iters 1000`).
