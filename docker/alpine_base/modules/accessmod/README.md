# AccessMod mobility kernel

`mobility.h` is the dependency-free contract shared by GRASS propagation
modules. It contains no raster or GRASS state and can therefore be compiled and
tested independently.

The model rationale, equations, assumptions, lookup-table values, and full
bibliography are recorded in [`bicycle-lut.md`](bicycle-lut.md).
The companion [`plot-bicycle-lut.R`](plot-bicycle-lut.R) script generates a
reference PNG using only base R.
`generate-bicycle-lut.R` is the canonical parameter source and produces the
diagnostic CSV and C header during the base-image build. These generated files
are intentionally not versioned.

## Encoded speed raster

AccessMod encodes transport mode and base speed in one raster value:

```text
encoded value = mode * 1,000,000 + speed_km_h * 1,000
```

The mode identifiers are 1 for walking, 2 for bicycling, and 3 for motorized
transport. The scenario speed remains the speed on flat terrain. Slope is a
ratio (`0.10` means 10 percent), distance is in metres, and the cost returned by
the kernel is in seconds.

Cycling uses a two-dimensional lookup table generated from a physical
speed/power model, an empirical downhill safety envelope, and a Tobler-shaped
hike-a-bike branch. The scenario value remains the exact flat-terrain speed.
The generated table is a build artefact; its readable canonical definition is
`generate-bicycle-lut.R`.

## Tests

The base-image build compiles every C file under
`r.walk.accessmod/tests/` with warnings treated as errors. It also runs a small
synthetic raster baseline comparing `r.walk.accessmod` with `r.cost` in standard
and knight-move modes.
