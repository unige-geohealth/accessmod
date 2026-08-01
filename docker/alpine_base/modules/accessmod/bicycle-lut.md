# AccessMod bicycle mobility model

## Model statement

AccessMod uses a **physics-based cycling speed model constrained by an
empirical downhill safety envelope**. The scenario table supplies the expected
flat-terrain speed for each bicycling class. This value is converted to an
equivalent cycling power and the physical model predicts speed on a slope.

For downhill travel, an empirical envelope prevents the physical model from
returning unsafe free-running speeds. Hike-a-bike becomes active wherever it is
faster than riding:

```text
physics = physical_speed(flat_speed, slope)
ride    = min(physics, flat_speed * downhill_envelope)
final   = max(ride, hike_a_bike)
```

The equations are evaluated by `generate-bicycle-lut.R` before the GRASS module
is compiled. Runtime code performs only bilinear interpolation over the
generated two-dimensional lookup table.

## Physical branch

Flat speed is converted to equivalent power with rolling and aerodynamic
resistance. The same power is then used to solve the force balance at the
requested slope. The fixed assumptions are:

| Parameter | Value |
| --- | ---: |
| Rider mass | 80 kg |
| Bicycle mass | 15 kg |
| Rolling-resistance coefficient | 0.012 |
| Frontal area | 0.445 m² |
| Transmission efficiency | 0.90 |
| Wind | 0 m/s |
| Temperature | 20 °C |
| Elevation | 500 m |

With these assumptions, representative conversions are approximately 97 W at
18 km/h, 117 W at 20 km/h, and 139 W at 22 km/h. These are equivalent modelling
powers, not measurements of an individual rider. A scenario speed can encode
the expected performance of a particular population and terrain class.

## Downhill safety envelope

The downhill envelope contains only four linearly interpolated anchors:

| Slope | Maximum riding factor | Basis |
| ---: | ---: | --- |
| 0% | 1.00 | Scenario flat speed |
| -5.5% | 1.20 | Flügel et al. peak-speed regime |
| -10% | 1.05 | Flügel et al. steep-descent braking regime |
| -25% | 0.00 | AccessMod non-ridable-slope assumption |

Flügel et al. identify a speed maximum around -5% to -6% and decreasing speed
on steeper descents, plausibly due to safety-related braking. Maurer et al.'s
Zurich observations independently corroborate pronounced braking below -10%,
but AccessMod does not derive a coefficient from their open-ended boxplot bin.
The -25% endpoint is an explicit routing assumption, not a published estimate.

## Hike-a-bike

The walking branch retains Tobler's curve shape. Its flat speed is
`min(scenario speed, 3.2 km/h)` and it has a 0.1 km/h numerical floor. The floor
is reduced for scenario speeds below 0.1 km/h so a positive scenario value is
not increased merely by the routing safeguard. There are no fixed dismount
thresholds: the fastest of riding and hike-a-bike is selected continuously.

## Compiled lookup table

The generated table covers:

- flat speed from 0 to 100 km/h at 0.5 km/h intervals;
- slope from -120% to +100% at 0.5 percentage-point intervals;
- 201 × 441 = 88,641 single-precision values;
- bilinear interpolation at runtime.

The generator validates the LUT against the exact model at every cell centre
over the operational 1–40 km/h range. The accepted limits are 0.35 km/h maximum
absolute error and 0.05 km/h at the 99th percentile. Inputs outside the slope
range are clamped defensively. AccessMod rejects bicycling speeds above
100 km/h before encoding the scenario raster.

The generated header and diagnostic CSV are build artefacts and are not stored
in Git. Recreate them with:

```sh
Rscript generate-bicycle-lut.R
Rscript plot-bicycle-lut.R /tmp/accessmod-bicycle-lut.png
```

The diagnostic CSV keeps the physical, safety-envelope, riding, hike-a-bike,
and selected branches separate so the model remains auditable.

## Scope and limitations

The model does not explicitly represent bicycle type, load, road surface,
traffic, curvature, weather, rider posture, or intersections. The scenario
speed absorbs these effects only approximately. Converting a class-specific
speed into equivalent power is a pragmatic AccessMod interpretation and must
not be described as physiological calibration.

## References

Flügel, S., Hulleberg, N., Fyhri, A., Weber, C., & Ævarsson, G. (2019).
Empirical speed models for cycling in the Oslo road network. *Transportation*,
46, 1395–1419. https://doi.org/10.1007/s11116-017-9841-8

Maurer, L. F., Meister, A., & Axhausen, K. W. (2025). Cycling speed profiles
from GPS data: Insights for conventional and electrified bicycles in
Switzerland. *Journal of Cycling and Micromobility Research*, 5, 100077.
https://doi.org/10.1016/j.jcmr.2025.100077

Tobler, W. (1993). *Three presentations on geographical analysis and modeling*.
NCGIA Technical Report 93-1. https://escholarship.org/uc/item/05r820mz
