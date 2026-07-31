# AccessMod mobility kernel

`mobility.h` is the dependency-free contract shared by GRASS propagation
modules. It contains no raster or GRASS state and can therefore be compiled and
tested independently.

## Encoded speed raster

AccessMod encodes transport mode and base speed in one raster value:

```text
encoded value = mode * 1,000,000 + speed_km_h * 1,000
```

The mode identifiers are 1 for walking, 2 for bicycling, and 3 for motorized
transport. The scenario speed remains the speed on flat terrain. Slope is a
ratio (`0.10` means 10 percent), distance is in metres, and the cost returned by
the kernel is in seconds.

The current bicycle function is explicitly named `am_bicycle_speed_legacy`.
Its characterization tests preserve the pre-LUT results and identify the steep
descent failure that the LUT must replace intentionally.

## Tests

The base-image build compiles every C file under
`r.walk.accessmod/tests/` with warnings treated as errors. It also runs a small
synthetic raster baseline comparing `r.walk.accessmod` with `r.cost` in standard
and knight-move modes.
