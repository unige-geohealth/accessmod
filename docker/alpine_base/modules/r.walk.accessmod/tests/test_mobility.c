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

static void expect_positive(const char *label, double actual)
{
    if (isfinite(actual) && actual > 0.0)
        return;

    fprintf(stderr, "%s: expected a finite positive value, got %.9f\n",
            label, actual);
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

    expect_close("walking normalized flat", am_walking_speed(3.2, 0.0),
                 3.2, 1e-12);

    /* The compiled LUT remains finite on extreme terrain. */
    expect_positive("steep descent speed",
                    am_bicycle_speed(12.0, -0.50));
    expect_positive("steep descent cost",
                    am_cost_seconds(bicycle, bicycle, 0.0, 0.0, -0.50,
                                    100.0, false, false, -1.0));

    /* Exact safety-envelope anchors and physical/hike-a-bike values. */
    expect_close("LUT flat", am_bicycle_speed(12.0, 0.0), 12.0, 0.0);
    expect_close("downhill peak", am_bicycle_speed(12.0, -0.055),
                 14.399999618530, 1e-9);
    expect_close("downhill minus 10", am_bicycle_speed(12.0, -0.10),
                 12.600000381470, 1e-9);
    expect_close("downhill minus 15", am_bicycle_speed(12.0, -0.15),
                 8.399999618530, 1e-9);
    expect_close("downhill non-riding anchor",
                 am_bicycle_speed(12.0, -0.25), 1.892977118492, 1e-9);
    expect_close("physical uphill before hike crossover",
                 am_bicycle_speed(12.0, 0.025), 4.798450469971, 1e-9);
    expect_close("uphill hike branch", am_bicycle_speed(12.0, 0.10),
                 2.255001783371, 1e-9);

    expect_close("zero base bicycle speed", am_bicycle_speed(0.0, 0.0),
                 0.0, 0.0);
    expect_close("negative base bicycle speed",
                 am_bicycle_speed(-1.0, 0.0), 0.0, 0.0);
    expect_close("maximum bicycle speed clamp",
                 am_bicycle_speed(101.0, 0.0), 100.0, 0.0);
    expect_close("lower slope clamp", am_bicycle_speed(12.0, -2.0),
                 am_bicycle_speed(12.0, AM_BICYCLE_SLOPE_MIN), 0.0);
    expect_close("upper slope clamp", am_bicycle_speed(12.0, 2.0),
                 am_bicycle_speed(12.0, AM_BICYCLE_SLOPE_MAX), 0.0);
    expect_close("linear interpolation helper",
                 am_linear_interpolate(0.25, 0.0, 10.0, 1.0, 20.0), 12.5,
                 0.0);

    for (int quarter_kmh = 1; quarter_kmh < 400; quarter_kmh += 2) {
        const double flat_speed = quarter_kmh / 4.0;
        expect_close("scenario speed preserved on flat terrain",
                     am_bicycle_speed(flat_speed, 0.0), flat_speed, 1e-6);
    }

    /* A low-power scenario reaches hike-a-bike sooner than a high-power one. */
    expect_close("low-power natural uphill crossing",
                 am_bicycle_speed(6.0, 0.025), 2.931900501251, 1e-9);
    expect_close("high-power physical uphill branch",
                 am_bicycle_speed(20.0, 0.025), 10.348983764648, 1e-9);

    for (int percent = -100; percent <= 100; percent++) {
        const double slope = percent / 100.0;
        const double bicycle_speed = am_bicycle_speed(12.0, slope);

        expect_positive("positive bicycle speed", bicycle_speed);
        if (bicycle_speed > 12.0 * 1.20) {
            fprintf(stderr, "unbounded bicycle speed at slope %.2f: %.9f\n",
                    slope, bicycle_speed);
            return EXIT_FAILURE;
        }
    }

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
