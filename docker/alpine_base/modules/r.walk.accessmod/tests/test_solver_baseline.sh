#!/bin/sh
set -eu

assert_raster_equal() {
  left="$1"
  right="$2"
  tolerance="$3"
  difference="baseline_delta_${left}_${right}"

  r.mapcalc --overwrite --quiet \
    expression="${difference} = abs(${left} - ${right})"
  max_difference="$(r.univar -g map="${difference}" | sed -n 's/^max=//p')"

  awk -v actual="${max_difference}" -v allowed="${tolerance}" \
    'BEGIN { exit !(actual <= allowed) }' || {
      echo "${left} and ${right} differ by ${max_difference}" >&2
      exit 1
    }
}

g.region n=3 s=0 e=3 w=0 res=1
r.mapcalc --quiet expression="baseline_elevation = 0"
r.mapcalc --quiet expression="baseline_speed = 3003600"
r.mapcalc --quiet expression="baseline_friction = 1"

r.walk.accessmod -s --quiet \
  elevation=baseline_elevation \
  friction=baseline_speed \
  output=baseline_walk_standard \
  start_coordinates=1.5,1.5
r.cost --quiet \
  input=baseline_friction \
  output=baseline_cost_standard \
  start_coordinates=1.5,1.5
r.mapcalc --quiet expression="baseline_expected_standard = \
  if(row() == 2 && col() == 2, 0, \
  if(row() == 2 || col() == 2, 1, sqrt(2)))"

assert_raster_equal baseline_walk_standard baseline_cost_standard 0
assert_raster_equal baseline_walk_standard baseline_expected_standard 1e-12

r.walk.accessmod -ks --quiet \
  elevation=baseline_elevation \
  friction=baseline_speed \
  output=baseline_walk_knight \
  start_coordinates=1.5,1.5
r.cost -k --quiet \
  input=baseline_friction \
  output=baseline_cost_knight \
  start_coordinates=1.5,1.5

assert_raster_equal baseline_walk_knight baseline_cost_knight 0
