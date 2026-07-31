## DESCRIPTION

*r.walk.accessmod* computes anisotropic cumulative travel time from one or more
starting locations. It extends [r.walk](r.walk.md) with the AccessMod
multimodal cost model and can also produce the direction towards the source for
use with [r.path](r.path.md).

The elevation raster provides the slope and direction of travel. The friction
raster and the AccessMod mode/speed inputs provide the time required to cross
each cell. Motorized modes ignore slope, while walking and cycling costs depend
on both slope and travel direction.

## NOTES

The module uses Dijkstra's algorithm. The accumulated travel-time raster is the
optimization criterion; the optional direction raster records the predecessor
of each reached cell.

With the knight-move flag, the propagation considers 16 neighbours instead of
8. This improves directional accuracy at the cost of additional processing.

The `memory` option controls the segment cache used for rasters larger than
available memory. NULL cells and cells rejected by the AccessMod cost model are
treated as barriers.

## EXAMPLE

```sh
g.region raster=elevation
r.walk.accessmod -k \
    elevation=elevation \
    friction=friction \
    output=travel_time \
    outdir=travel_direction \
    start_coordinates=635576,216485
```

## SEE ALSO

[r.cost](r.cost.md), [r.walk](r.walk.md), [r.path](r.path.md),
[r.mapcalc](r.mapcalc.md)

## AUTHORS

Based on *r.cost* and *r.walk* from GRASS GIS. Modified for AccessMod 5 by Fred
Moser.
