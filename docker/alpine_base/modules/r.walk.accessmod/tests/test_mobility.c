#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "../../accessmod/mobility.h"

static void expect_close(const char *label, double actual, double expected,
                         double tolerance)
{
    if (fabs(actual - expected) <= tolerance)
        return;

    fprintf(stderr, "%s: expected %.9f, got %.9f\n", label, expected,
            actual);
    exit(EXIT_FAILURE);
}

static void expect_equal_int(const char *label, int actual, int expected)
{
    if (actual == expected)
        return;

    fprintf(stderr, "%s: expected %d, got %d\n", label, expected, actual);
    exit(EXIT_FAILURE);
}

int main(void)
{
    const double bicycle = 2012000.0;
    const double motorized = 3003600.0;

    /* Scenario raster encoding is a public contract shared with R. */
    expect_equal_int("bicycle mode", am_decode_mode(bicycle),
                     AM_MODE_BICYCLING);
    expect_close("bicycle base speed", am_decode_speed(bicycle), 12.0,
                 0.0);
    expect_equal_int("motorized mode", am_decode_mode(motorized),
                     AM_MODE_MOTORIZED);
    expect_close("motorized base speed", am_decode_speed(motorized), 3.6,
                 1e-12);

    /* Characterization values for the model predating the bicycle LUT. */
    expect_close("bicycle flat", am_bicycle_speed_legacy(12.0, 0.0),
                 12.000213, 2e-6);
    expect_close("bicycle 10 percent uphill",
                 am_bicycle_speed_legacy(12.0, 0.10),
                 1.604894, 2e-6);
    expect_close("bicycle 9 percent downhill cap",
                 am_bicycle_speed_legacy(12.0, -0.09), 24.0, 0.0);
    expect_close("walking normalized flat", am_walking_speed(3.2, 0.0),
                 3.2, 1e-12);

    /* Known legacy defect: Newton fails on very steep descents. The LUT
     * change must replace this assertion with a positive hike-a-bike cost. */
    expect_close("legacy steep descent speed",
                 am_bicycle_speed_legacy(12.0, -0.50), 0.0, 0.0);
    expect_close("legacy steep descent sentinel cost",
                 am_cost_seconds(bicycle, bicycle, 0.0, 0.0, -0.50, 100.0,
                                 false, false, -1.0),
                 -3600.0, 0.0);

    /* A constant motorized speed must be independent of slope and neighbor
     * mode, for both standard and knight moves. */
    expect_close("motorized standard move",
                 am_cost_seconds(motorized, motorized, 0.0, 0.0, 0.30,
                                 100.0, false, false, -1.0),
                 100.0, 1e-12);
    expect_close("motorized knight move",
                 am_cost_seconds(motorized, motorized, motorized, motorized,
                                 0.30, 100.0, true, false, -1.0),
                 100.0, 1e-12);

    /* Reverse routing changes only the sign of the slope. */
    expect_close("reverse slope",
                 am_cost_seconds(bicycle, bicycle, 0.0, 0.0, 0.10, 100.0,
                                 false, true, -1.0),
                 am_cost_seconds(bicycle, bicycle, 0.0, 0.0, -0.10, 100.0,
                                 false, false, -1.0),
                 0.0);

    return EXIT_SUCCESS;
}
