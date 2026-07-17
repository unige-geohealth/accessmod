#include <math.h>
#include <stdio.h>
#include <stdlib.h>

#include "../accessmod/c/bicycleSpeed.h"

static void expect_close(const char *label, float actual, float expected,
                         float tolerance)
{
    if (fabsf(actual - expected) <= tolerance)
        return;

    fprintf(stderr, "%s: expected %.6f, got %.6f\n", label, expected, actual);
    exit(EXIT_FAILURE);
}

int main(void)
{
    const float flat_speed = bicycleSpeed(12.0f, 0.0f);
    const float uphill_speed = bicycleSpeed(12.0f, 0.10f);

    expect_close("flat slope", flat_speed, 12.0f, 0.05f);
    expect_close("10 percent uphill ratio", uphill_speed, 1.6049f, 0.01f);

    if (!(uphill_speed < flat_speed)) {
        fprintf(stderr, "10 percent uphill must be slower than flat terrain\n");
        return EXIT_FAILURE;
    }

    return EXIT_SUCCESS;
}
