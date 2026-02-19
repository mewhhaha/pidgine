# Benchmark Focus

Current performance target set:

- `program/10k/eachm`
- `program/10k/eachm-aztecs`
- `program/10k+1/eachm` (informational only)
- `program/10k+1/eachm-aztecs` (informational only)

Use `cabal bench` directly (no `CABAL_DIR` override).

Time runs:

```sh
cabal bench pidgine-bench --ghc-options=-O2 --benchmark-options='-m glob <benchmark-name> +RTS -N -s -RTS'
```

Allocation comparisons:

```sh
cabal bench pidgine-bench --ghc-options=-O2 --benchmark-options='-m glob <benchmark-name> --iters 1000 +RTS -N -s -RTS'
```

Notes:

- `+RTS -N -s -RTS` enables parallel runtime and RTS allocation stats.
- For fair allocation per-op comparisons, use fixed iteration count (currently `--iters 1000`).

# Kernel Goal

Execution model target:

- One fused batch kernel per `await <batch>` boundary.
- Kernel runs over the main spine only (no archetype storage paths).
- Assume at least one full pass over all entities per tick; optimize that path first.
- Keep evaluation order deterministic (stable program/step order).
- Skip global `stepWorld` unless step components actually exist in the world.
- Keep purity at API boundaries (no mutable state leakage).

Optimization principles:

- Prefer fewer world traversals over specialized micro-paths.
- Prefer simpler code when performance is within target budgets.
- Use `program/10k/eachm` as the primary optimization benchmark.
- Use `program/10k+1/eachm` as a secondary regression check only.

# API Cleanup

- Remove `programWith` (aim to keep only the auto-handle `program` path).
