# Bicycle Model Lookup Table

This note summarizes a proposed change for the bicycle speed model in
`r.walk.accessmod`: keep the physics-based bicycle model where cycling is
plausible, fall back to walking speed where the cyclist is likely to dismount,
and replace repeated per-cell Newton solves with a lookup table.

The current AccessMod bicycle function was originally adapted from
BikeCalculator.com, with permission, then rewritten in C for `r.walk.accessmod`.
The calculator estimates power from speed and predicts speed from power using
parameters such as grade, rider weight, bicycle weight, tire type, riding
position, head wind, temperature, elevation, and transmission efficiency.

## Why change the current model?

The current bicycle model computes a physics approximation for every movement
cost. This is accurate in ordinary riding conditions, but it has two practical
issues:

- it is expensive because the nonlinear equation is solved repeatedly during
  cost expansion;
- on unusual slopes, especially steep downhill slopes, the physics equation does
  not model braking, control, road surface, or the decision to dismount.

For example, the current wrapper caps downhill speed at `2 * flat_speed`. This
avoids unrealistic downhill values, but also creates a long artificial plateau
and can abruptly drop to zero when the raw solver result becomes negative. This
is a model-behavior issue, not a lookup table issue.

## Proposed hybrid model

The proposed routing curve is:

```text
bicycle speed =
  physics bicycle speed          for slopes where riding is plausible
  Tobler-style walking speed     where the cyclist pushes the bicycle
  near-zero / blocked speed      outside the proposed rideability range
```

Default thresholds used in this research script:

| Slope range | Proposed behavior |
| --- | --- |
| `<= -25%` | near-zero / blocked bicycle routing |
| `-25% .. -18%` | transition to walking the bike downhill |
| `-18% .. -5%` | transition from walking-bike speed to bicycle physics |
| `-5% .. +20%` | current physics-based bicycle speed |
| `+20% .. +25%` | transition from bicycle physics to walking-bike speed |
| `+25% .. +35%` | walking the bike uphill, then transition to near-zero |
| `>= +35%` | near-zero / blocked bicycle routing |

These are proposed defaults for discussion and sensitivity analysis, not claimed
universal constants. The cycling-physics part is based on the existing
BikeCalculator-derived model. The dismount and blocked thresholds are AccessMod
modelling assumptions and should remain configurable/tested.

## Lookup table idea

The lookup table would approximate:

```text
hybrid_bicycle_speed = f(flat_speed, local_slope)
```

It does not need to list all slopes observed in a study area. `r.walk.accessmod`
expands cells using cumulative cost and evaluates only candidate movements. The
lookup table is therefore just a fast replacement for calling the same speed
function repeatedly.

An implementation in `timeCostManager.h` could be:

```text
if bicycle mode:
  if slope is outside the rideable physics range:
    use Tobler / walk-bike fallback, or near-zero if blocked
  else:
    use bicycle lookup table with linear interpolation
```

With the default `100` slope samples over `-40% .. +40%`, interpolation error is
small while avoiding the repeated Newton solve. The generated figure compares:

- the current AccessMod bicycle function;
- the proposed hybrid physics/walk-bike curve;
- a `100` point lookup-table approximation of the hybrid curve.

## Run

```sh
uv run python docker/alpine_base/modules/r.walk.accessmod/look_up_table/bicycle_lookup_analysis.py
```

Outputs are written to:

```text
docker/alpine_base/modules/r.walk.accessmod/look_up_table/output/
```

Main outputs:

- `hybrid_bicycle_lookup_flat_12.svg`: current function, hybrid curve, and
  100-point lookup approximation.
- `hybrid_lookup_error_stats.csv`: lookup error statistics.
- `hybrid_lookup_error_flat_12.svg`: absolute interpolation error by slope.
- `current_model_diagnostics.csv`: current-model plateau/discontinuity summary.
- `model_curves_flat_12.csv`: dense values used for the figure.

No third-party Python package is required.

## Public sources

- BikeCalculator.com, "Estimate your Power from Speed":
  https://bikecalculator.com/wattsMetric.html
- BikeCalculator.com, "Predict speed from power":
  https://bikecalculator.com/veloMetric.html
- Bos et al. (2020), "On maximizing VAM for a given power", open arXiv paper:
  https://arxiv.org/abs/2006.15816
- Tobler hiking function summary and formula, used as the walking-speed basis
  for dismounted bicycle travel:
  https://en.wikipedia.org/wiki/Tobler%27s_hiking_function
