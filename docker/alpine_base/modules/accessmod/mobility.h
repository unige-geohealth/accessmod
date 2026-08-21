#ifndef ACCESSMOD_MOBILITY_H
#define ACCESSMOD_MOBILITY_H

#include <stdbool.h>
#include <math.h>

/*
 * Shared AccessMod mobility kernel.
 *
 * It provides one dependency-free contract which can be included by
 * r.walk.accessmod and the future r.accessmod module. Bicycle routing uses a
 * generated two-dimensional LUT compiled from the physical model and its
 * empirical downhill safety envelope.
 */

enum am_transport_mode {
    AM_MODE_WALKING = 1,
    AM_MODE_BICYCLING = 2,
    AM_MODE_MOTORIZED = 3
};

#define AM_MODE_ENCODING_SCALE 1000000.0
#define AM_SPEED_ENCODING_SCALE 1000.0

/* Generated during the base-image build by generate-bicycle-lut.R. */
#include "bicycle-lut.generated.h"

static inline double am_linear_interpolate(double x, double x0, double y0,
                                           double x1, double y1)
{
    return y0 + (x - x0) * (y1 - y0) / (x1 - x0);
}

static inline int am_decode_mode(double encoded_mode_speed)
{
    return (int)floor(encoded_mode_speed / AM_MODE_ENCODING_SCALE);
}

static inline double am_decode_speed(double encoded_mode_speed)
{
    const int mode = am_decode_mode(encoded_mode_speed);

    return (encoded_mode_speed - mode * AM_MODE_ENCODING_SCALE) /
           AM_SPEED_ENCODING_SCALE;
}

static inline double am_walking_speed(double speed, double slope)
{
    const double top_speed = speed / exp(-0.175);

    return exp(-3.5 * fabs(slope + 0.05)) * top_speed;
}

static inline double am_bicycle_speed(double speed, double slope)
{
    unsigned int speed_index;
    unsigned int slope_index;
    double speed_position;
    double slope_position;
    double speed_fraction;
    double slope_fraction;
    double lower;
    double upper;

    if (!isfinite(slope) || speed <= 0.0)
        return 0.0;

    if (speed > AM_BICYCLE_SPEED_MAX)
        speed = AM_BICYCLE_SPEED_MAX;
    if (slope < AM_BICYCLE_SLOPE_MIN)
        slope = AM_BICYCLE_SLOPE_MIN;
    else if (slope > AM_BICYCLE_SLOPE_MAX)
        slope = AM_BICYCLE_SLOPE_MAX;

    speed_position =
        (speed - AM_BICYCLE_SPEED_MIN) / AM_BICYCLE_SPEED_STEP;
    slope_position =
        (slope - AM_BICYCLE_SLOPE_MIN) / AM_BICYCLE_SLOPE_STEP;
    speed_index = (unsigned int)floor(speed_position);
    slope_index = (unsigned int)floor(slope_position);

    if (speed_index >= AM_BICYCLE_SPEED_COUNT - 1) {
        speed_index = AM_BICYCLE_SPEED_COUNT - 2;
        speed_fraction = 1.0;
    }
    else {
        speed_fraction = speed_position - speed_index;
    }

    if (slope_index >= AM_BICYCLE_SLOPE_COUNT - 1) {
        slope_index = AM_BICYCLE_SLOPE_COUNT - 2;
        slope_fraction = 1.0;
    }
    else {
        slope_fraction = slope_position - slope_index;
    }

    lower = am_linear_interpolate(
        speed_fraction, 0.0,
        am_bicycle_speed_lut[speed_index][slope_index], 1.0,
        am_bicycle_speed_lut[speed_index + 1][slope_index]);
    upper = am_linear_interpolate(
        speed_fraction, 0.0,
        am_bicycle_speed_lut[speed_index][slope_index + 1], 1.0,
        am_bicycle_speed_lut[speed_index + 1][slope_index + 1]);

    return am_linear_interpolate(slope_fraction, 0.0, lower, 1.0, upper);
}

static inline double am_motorized_speed(double speed, double slope)
{
    (void)slope;
    return speed;
}

static inline double am_mobility_speed(int mode, double speed, double slope)
{
    switch (mode) {
    case AM_MODE_WALKING:
        return am_walking_speed(speed, slope);
    case AM_MODE_BICYCLING:
        return am_bicycle_speed(speed, slope);
    case AM_MODE_MOTORIZED:
        return am_motorized_speed(speed, slope);
    default:
        return 0.0;
    }
}

static inline double am_cost_seconds(
    double encoded_mode_speed, double encoded_mode_speed_adjacent_1,
    double encoded_mode_speed_adjacent_2,
    double encoded_mode_speed_adjacent_3, double slope, double distance,
    bool knight, bool return_path, double null_value)
{
    double cost;
    const double distance_divisor = distance / (knight ? 4000.0 : 2000.0);

    if (return_path)
        slope = -slope;

    const double current_cost =
        1.0 / (am_mobility_speed(am_decode_mode(encoded_mode_speed),
                                 am_decode_speed(encoded_mode_speed), slope) /
               distance_divisor);
    const double adjacent_1_cost =
        1.0 /
        (am_mobility_speed(am_decode_mode(encoded_mode_speed_adjacent_1),
                           am_decode_speed(encoded_mode_speed_adjacent_1),
                           slope) /
         distance_divisor);

    cost = current_cost + adjacent_1_cost;

    if (knight) {
        const double adjacent_2_cost =
            1.0 /
            (am_mobility_speed(am_decode_mode(encoded_mode_speed_adjacent_2),
                               am_decode_speed(encoded_mode_speed_adjacent_2),
                               slope) /
             distance_divisor);
        const double adjacent_3_cost =
            1.0 /
            (am_mobility_speed(am_decode_mode(encoded_mode_speed_adjacent_3),
                               am_decode_speed(encoded_mode_speed_adjacent_3),
                               slope) /
             distance_divisor);

        cost += adjacent_2_cost + adjacent_3_cost;
    }

    if (cost == INFINITY)
        cost = null_value;

    return cost * 3600.0;
}

#endif /* ACCESSMOD_MOBILITY_H */
