# AccessMod 5 — Developer Guide

AccessMod models physical accessibility to health services (travel time, catchments, coverage).
It runs as a Shiny web app tightly coupled with GRASS GIS 8, packaged entirely in a Docker image.
Stack: R + Shiny, GRASS 8, SQLite, GDAL, Node.js (build tooling only).

New here? Start with `README.md` for user context, then come back to this file for the internals.

## Commands

```sh
npm run test          # Full test suite (Docker named volume, CI-compatible)
npm run dev           # Source run.r in running container (R session)
docker compose up     # Start dev stack (app at localhost:3080 / :3180)
npm run version       # Bump version tag (run from staging or main)
```

Tests write results to `/tmp/tests.json`. All 24 checks must pass.

## Development workflow

```sh
# Launch the dev stack
# App files are bind-mounted from repo root -> /app (see docker-compose.yml)
docker compose up

# Two ports are exposed:
#   localhost:3080  -> container:3000  direct R/Shiny session (dev, see below)
#   localhost:3180  -> container:3100  shiny-manager session (main managed process)
#
# Session session healthcheck:
#   http://localhost:3080/health


# --- Dev session (hot-reload workflow) ---
# Starts Shiny directly on port 3000 -> localhost:3080
# Bypasses shiny-manager; reload with source('run.r') as needed.
npm run dev
# OR interactively:
docker compose exec am5_dev R
> source('run.r')


# --- Replay analysis (dev) ---
docker compose exec am5_dev R
> source('global.R')
> amAnalysisReplayExec("<path to config>.json")
# example:
> amAnalysisReplayExec("/data/dbgrass/demo/demo/accessmodConfigs/lAnalysisParameters__425.json")


# --- Build images ---
cd docker
./build.sh


# --- Tests (see tests/README.md for structure and patterns) ---
npm run test
# - or - direct command with docker compose
docker compose exec am5_dev Rscript tests/start.R
# - or - from an interactive session
docker compose exec am5_dev R
> source('tests/start.R')
```

## Local shared workspace

Use `_shared/` for temporary investigation scripts, issue reproductions, downloaded fixtures, and generated comparison outputs.
This directory is local-only and should stay ignored by git.

Preferred pattern:

- keep issue-specific work under `_shared/<issue-or-topic>/`
- write reproducible scripts there instead of scattering scratch code in app folders
- keep outputs small and human-readable when they may be pasted into GitHub issues
- use bind mounts or explicit copies when container access is needed
- do not create Docker named volumes for ad hoc scratch data unless there is a specific reason
- avoid commands that remove or recreate Docker volumes during investigations

## Architecture

```
global.R              → sources config-app.R, then all tools/R/*.R, then memoise
config/config-app.R   → single source of truth for paths, separators, class table
tools/R/              → 46 R files: GRASS wrappers, analysis, data management, UI
modules/              → 15 Shiny modules (one per analysis type / UI section)
tests/                → AmTests harness; see tests/README.md
docker/alpine_base/   → demo data baked into image at /data/dbgrass
```

Initialization order matters: `config-app.R` must load before any `tools/R/` file is sourced.

## Layer naming convention

Every GRASS/SQLite layer follows: `{class}__{tags}@{mapset}`

- `__` (double underscore) = `config$sepClass` — splits class from tags
- `_` (single underscore) = `config$sepTagFile` — splits tags from each other
- `@` = mapset suffix

Examples: `rDem__dem@PERMANENT`, `vFacility__demo@demo`, `rTravelTime__motorized@p003`

Intermediate computation layers use `tmp__` (double underscore, e.g. `tmp__raster_cat`) — these are valid convention layers and are **not** auto-cleaned; analysis code removes them explicitly via `rmRastIfExists("tmp__*")` or `on_exit_add`.

**Classes** are defined in `www/dictionary/classes.json` (~50 entries). Prefix letter encodes type:
`r` = raster (GRASS), `v` = vector (GRASS), `t` = table (SQLite), `l` = config/log.

Layers without `__` are treated as unnamed: `amUpdateDataListObject()` removes them at startup via `rmVectIfExists("^tmp_*")` etc., and `amCreateSelectList()` marks any remaining ones as `NA` and deletes them.
**Never create or reference a layer that lacks the double-underscore separator.**

## Hardcoded paths — do not move

| Path | Purpose |
|------|---------|
| `/data/dbgrass/` | GRASS database root (`config$pathGrassDataBase`) |
| `/data/cache/` | Memoised function cache (`config$pathCacheDir`) |
| `/data/logs/` | GRASS session logs (`config$pathGrassHome`) |
| `docker/alpine_base/data/` | Demo data source (matches image content) |

`config$pathGrassDataBase` is hardcoded to `/data/dbgrass/`. If you change `GISDBASE` without also changing this, you get split-brain: `amGrassNS()` uses env, project validation uses config. **Don't diverge these.**

## GRASS session model

GRASS state is managed via temporary `.gisrc` files, not persistent env vars.
`amGrassNS(location, mapset, { expr })` — wraps any GRASS operation in a namespace.
Never call GRASS functions outside an `amGrassNS` block in analysis code.

## Replay / config system

Analysis configs are saved as JSON to `accessmodConfigs/` and can be replayed.
Three-phase: **save** (`amAnalysisReplaySave`) → **validate** (`amAnalysisReplayValidateConf`) → **execute** (`amAnalysisReplayExec`).
Validation rules live in `tools/R/amAnalysisReplayValidationDict.json`. When adding a new editable arg to any analysis, add it to this dict.

## Testing strategy

- No testthat, no tidyverse — custom `AmTests` R6 class (~100 lines).
- Tests run inside the exact production Docker image (zero framework overhead).
- Integration suites use pre-packaged demo data; regression via xlsx reference files.
- Unit tests live in `tests/unit/` (pure R, no GRASS needed).
- `tests/start.R` is the entry point; runs unit tests first, then integration suites in order.
- See `tests/README.md` for patterns.

## Branches

- `main`: production-ready code; full release versioning runs from here.
- `staging`: integrates new features and minor versions.

### Expected workflow

1. Develop in a dedicated feature branch.
2. Test the feature.
3. Merge into `staging`.
4. Promote to `main` when ready to release.

## Versioning

AccessMod uses a project-scoped semver convention where the major is **frozen at 5**:

| Segment | Meaning | Example |
|---------|---------|---------|
| `5` | Product generation — never changes | — |
| `5.X` | Compatibility break — requires new VM or Electron app | `5.8` → `5.9` |
| `5.x.X` | Regular fix or feature — safe to update in-place | `5.9.0` → `5.9.1` |

Pre-releases use `5.x.x-alpha.N` / `5.x.x-beta.N` (run from `staging`), stable from `main`.

Run `npm run version` to bump interactively. Commit messages must follow conventional commit format (`fix:`, `feat:`, `chore:`, etc.) — enforced by commitlint on every commit.

> **Future:** migrate `npm run version` to [release-it](https://github.com/release-it/release-it) with `@release-it/bumper` for multi-file updates and a `before:bump` hook to enforce the frozen major. The current custom `version_manager/` script is functional but has known gaps (see inline comments).

## Electron

```sh
cd electron
yarn start
# yarn start:debug — interactive session with external debugger
```

## Landmines

- **overlayfs + GRASS rename**: GRASS overwrites a vector by renaming its directory. overlayfs blocks directory renames on image lower layers. Tests must mount a writable Docker named volume at `/data/dbgrass` (see `test.sh`). Never attempt to write to baked-in image layers.
- **`amRandomName` bug**: `letters[round(runif(n) * 24)]` — `round()` can produce `0`, and `letters[0]` = `character(0)` in R, silently dropping a character. Output length is not guaranteed to equal `n`.
- **`config$sepClass` in regex**: `__` must be escaped when used in regex — use `paste0("\\", config$sepClass)` or `fixed = TRUE`.
- **`amNoDataCheck` requires config**: depends on `config$defaultNoData` — not a pure function despite looking like one.
- **Location == mapset == project name**: these three are always identical. Creating a project with a mismatched name will break path resolution.
- **Demo shapes are NOT GRASS vectors**: `vCatchment__demo`,  etc. in `accessmodShapes/` are shapefiles read via `amGetShapesList()`, not GRASS vectors. `amVectExists()` returns FALSE for them.
- **Polygon vector import must not use `write_VECT()`**: rgrass imports polygon `SpatVector`s with `v.in.ogr type=boundary`. For valid administrative boundary layers with tiny overlaps/slivers, this can create stray line primitives and later make `v.out.ogr -m` fail with `Mixing IDs of areas and primitives`. Use the AccessMod polygon import path in `amUploadVector()` (temporary GPKG + `v.in.ogr` without `type`) and keep accepted vector geometries declared in `www/dictionary/classes.json`.
