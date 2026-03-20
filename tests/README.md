# AccessMod Tests

## Running

```bash
npm run test          # Docker run form repo root (clean named volume, CI-compatible)
```

Or inside a running container:
```bash
docker compose exec am5_dev Rscript tests/start.R
```

## Structure

```
tests/
├── start.R                        # Entry point — sources all suites in order
├── helpers.R                      # replayExec / replayImport utilities
├── unit/
│   └── test_amRandomName.R        # Template for pure-function unit tests
├── project_crud/
│   └── test_demo.R                # Create project, import raster/vector/table, delete
├── accessibility/
│   └── test_demo_motorized.R      # Travel-time analysis (2×2 knight/isotropic matrix)
├── referral/
│   └── test_demo.R                # Referral pathways, permutation, parallel execution
└── best_coverage/
    └── test_demo.R                # Facility selection optimisation, admin constraint
```

## How it works

`start.R` sources `global.R` (full app init), then runs each suite via `amtest$script()`.
Each suite calls `amtest$check(description, condition)` from the shared `AmTests` instance.
Results are written as JSON (`pass: true/false` + per-test detail).

Integration suites use pre-packaged demo data from `docker/alpine_base/data/demo/demo/`.
Reference outputs (xlsx) live in `tests/<suite>/data/` and are compared with `all_equal`.

## Adding a test

- **Unit test:** add a file under `tests/unit/`, wire it in `start.R` before the integration suites.
- **Integration test:** follow the `replayExec` / `replayImport` pattern in an existing suite.
