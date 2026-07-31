#ifndef ACCESSMOD_MOBILITY_H
#define ACCESSMOD_MOBILITY_H

#include <stdbool.h>
#include <math.h>

/*
 * Shared AccessMod mobility kernel.
 *
 * This header deliberately preserves the legacy numerical model. It provides
 * one dependency-free contract which can be included by r.walk.accessmod and
 * the future r.accessmod module. The bicycle implementation will be replaced
 * by the validated LUT in a separate, explicitly reviewed change.
 */

enum am_transport_mode {
    AM_MODE_WALKING = 1,
    AM_MODE_BICYCLING = 2,
    AM_MODE_MOTORIZED = 3
};

#define AM_MODE_ENCODING_SCALE 1000000.0
#define AM_SPEED_ENCODING_SCALE 1000.0

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

static inline float am_bicycle_newton(float aero, float headwind,
                                      float tire_resistance,
                                      float transmission, float power)
{
    const int max_iterations = 10;
    float velocity = 20;
    float tolerance = 0.05;

    for (int i = 1; i < max_iterations; i++) {
        float total_velocity = velocity + headwind;
        float value =
            velocity *
                (aero * total_velocity * total_velocity + tire_resistance) -
            transmission * power;
        float derivative =
            aero * (3.0 * velocity + headwind) * total_velocity +
            tire_resistance;
        float next_velocity = velocity - value / derivative;

        if (fabs(next_velocity - velocity) < tolerance)
            return next_velocity;

        velocity = next_velocity;
    }

    return 0.0f;
}

static inline float am_bicycle_speed_physics(float speed, float slope)
{
    const int slope_flat = 0;
    const int rider_weight = 80;
    const int bicycle_weight = 15;
    const float rolling_resistance = 0.012;
    const float frontal_area = 0.445;
    const float wind_speed = 0;
    const float temperature = 20;
    const float elevation = 500;
    const float transmission_efficiency = 0.90;
    float air_density =
        (1.293 - 0.00426 * temperature) * exp(-elevation / 7000.0);
    float total_weight = 9.8 * (rider_weight + bicycle_weight);
    float air_resistance = 0.5 * frontal_area * air_density;
    float bicycle_speed = speed / 3.6;
    float flat_resistance =
        total_weight * (slope_flat + rolling_resistance);
    float slope_resistance =
        total_weight * (slope + rolling_resistance);
    float total_speed = bicycle_speed + wind_speed;
    float flat_power =
        (bicycle_speed * flat_resistance +
         bicycle_speed * total_speed * total_speed * air_resistance) /
        transmission_efficiency;

    bicycle_speed =
        am_bicycle_newton(air_resistance, wind_speed, slope_resistance,
                          transmission_efficiency, flat_power) *
        3.6;

    return bicycle_speed;
}

static inline double am_bicycle_speed_legacy(double speed, double slope)
{
    double final_speed = am_bicycle_speed_physics((float)speed, (float)slope);

    if (final_speed >= speed * 2.0)
        final_speed = speed * 2.0;

    if (final_speed < 0.0)
        final_speed = 0.0;

    return final_speed;
}

static inline double am_walking_speed(double speed, double slope)
{
    const double top_speed = speed / exp(-0.175);

    return exp(-3.5 * fabs(slope + 0.05)) * top_speed;
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
        return am_bicycle_speed_legacy(speed, slope);
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
