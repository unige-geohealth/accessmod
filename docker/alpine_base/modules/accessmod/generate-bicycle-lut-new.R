#!/usr/bin/env Rscript

# =============================================================================
# Canonical source for the AccessMod bicycle speed-factor model.
#
# Generates two artefacts:
#   bicycle-lut.csv            inspection / article table
#   bicycle-lut.generated.h    consumed by mobility.h
#
# -----------------------------------------------------------------------------
# MODEL
# -----------------------------------------------------------------------------
# The user supplies a flat speed V_flat (km/h) per landcover class in the travel
# scenario. This model supplies only the dimensionless response to gradient, so
# that v(S) = V_flat * factor(S), floored by the speed of pushing the bicycle.
#
#   S = dz/dx along the direction of travel, positive uphill (Tobler convention)
#
#   factor(S) =
#     | max(0, 1 - K_UPHILL * S)                        S >= 0
#     | 1 + K_DOWNHILL * |S|                            SLOPE_PEAK <= S < 0
#     | F_peak - K_BRAKE * (|S| - |SLOPE_PEAK|)         SLOPE_STEEP <= S < SLOPE_PEAK
#     | F_steep - K_BRAKE_STEEP * (|S| - |SLOPE_STEEP|) S < SLOPE_STEEP
#
#   push(S)  = min(V_flat, HIKE_FLAT_SPEED)
#              * exp(-TOBLER_K * |S + TOBLER_S0|) / exp(-TOBLER_K * TOBLER_S0)
#
#   v(S)     = max(V_flat * factor(S), push(S))
#
# Three straight lines on the descent side, one on the climb side, and a Tobler
# floor. There is deliberately NO dismount threshold parameter: the gradient at
# which riding stops is where the ride branch crosses the push branch, which
# therefore moves with the user's V_flat (about 9.5% uphill for a 6 km/h class,
# about 14% for a 20 km/h class). That behaviour is a consequence of the two
# published curves intersecting, not a tuned constant.
#
# -----------------------------------------------------------------------------
# SOURCES
# -----------------------------------------------------------------------------
# Parkin, J. & Rotheram, J. (2010). Design speeds and acceleration
#   characteristics of bicycle traffic for use in planning, design and
#   appraisal. Transport Policy 17(5), 335-341. doi:10.1016/j.tranpol.2010.03.001
#   GPS study of 16 commuter cyclists in Leeds, UK. Linear regression of speed
#   on gradient: v = 6.01 - 40.02 * S_up - 23.79 * S_down (m/s, S fractional,
#   downhill negative). Normalising by the 6.01 m/s intercept gives the
#   dimensionless slopes 40.02/6.01 = 6.66 and 23.79/6.01 = 3.96 used here.
#   Observed range approximately -8% to +9%.
#
# Flugel, S., Hulleberg, N., Fyhri, A., Weber, C. & Aevarsson, G. (2019).
#   Empirical speed models for cycling in the Oslo road network.
#   Transportation 46, 1395-1419. doi:10.1007/s11116-017-9841-8
#   Log-speed regression coefficients by gradient class, with controls for
#   infrastructure, preceding-link gradient and rider demographics. Speed peaks
#   at -5 to -6% and declines on steeper descents (braking). exp(beta) class
#   means, relative to flat, are tabulated in the validation block below.
#
# Tobler, W. (1993). Three presentations on geographical analysis and modeling.
#   NCGIA Technical Report 93-1. Walking speed W = 6 exp(-3.5 |S + 0.05|) km/h.
#   Used here only for the SHAPE of the push branch; the flat-ground push speed
#   is set by HIKE_FLAT_SPEED, which is below Tobler's 5 km/h to account for
#   manoeuvring a loaded bicycle on foot.
#
# Maurer, L.F., Meister, A. & Axhausen, K.W. (2025). Cycling speed profiles from
#   GPS data: insights for conventional and electrified bicycles in Switzerland.
#   Journal of Cycling and Micromobility Research 5, 100077.
#   doi:10.1016/j.jcmr.2025.100077
#   Used for validation only. See the note below.
#
# -----------------------------------------------------------------------------
# VALIDATION AGAINST MAURER ET AL. 2025 (ZURICH)
# -----------------------------------------------------------------------------
# The underlying data is declared confidential by the authors, so the following
# medians were read off the conventional-bicycle boxplot (edge mean speed by
# gradient class) with WebPlotDigitizer. They are approximations and are used
# for corroboration only, never to set a coefficient.
#
#   class        bin centre   median km/h   factor (/ the -2..2 bin)
#   -----------  ----------   -----------   ------------------------
#   < -10             open          14.11                     0.669
#   -10 to -6           -8          29.70                     1.408
#   -6 to -2            -4          26.46                     1.254
#   -2 to 2              0          21.10                     1.000
#   2 to 6               4          13.37                     0.634
#   6 to 10              8          10.25                     0.486
#   > 10              open           6.13                     0.291
#
# Two caveats govern how far these can be pushed. The bins are four points wide,
# so adjacent bins cannot resolve a local slope (0->4% implies 9.16, 4->8%
# implies 3.69); and the two outer classes are open-ended, so they have no
# defined gradient at all. Unlike Flugel's regression coefficients these are
# unconditional medians, which likely inflates the descent factors (steep
# descents in Zurich are disproportionately main roads) and deflates the near-
# uphill factors (steep climbs are disproportionately short residential ramps
# ending at junctions).
#
# One statistic is robust to all of that, because it falls inside a closed bin
# and needs only interpolation between two observed points: the gradient at
# which speed halves.
#
#   Zurich, interpolated between the 4% and 8% bins   7.62%
#   Parkin & Rotheram, K_UPHILL = 6.66                7.51%
#
# Independent confirmation of K_UPHILL from a different country and decade. A
# second, weaker check: the "> 10" bin sits at factor 0.291, which under
# K_UPHILL = 6.66 corresponds to a mean gradient of 10.6% -- a plausible mean
# for that class.
#
# The "< -10" bin is the reason K_BRAKE_STEEP exists. Extending Flugel's
# -6%..-10% trend (K_BRAKE = 3.68) beyond the observed range predicts a factor
# of about 0.87 at -15%, whereas Zurich observes 0.669 for the whole class.
# K_BRAKE_STEEP is set so the fourth segment passes through that median on the
# assumption that the class mean gradient is -15%. This is the least
# well-founded number in the file: a class mean of -13% would imply 12.8 and one
# of -18% would imply 4.8. It is nonetheless preferable to the linear extension,
# which produced no descent dismount at all.
#
# -----------------------------------------------------------------------------
# KNOWN LIMITATIONS
# -----------------------------------------------------------------------------
# 1. Every source is urban European cycling on geared bicycles with flat speeds
#    of 15-22 km/h. Applying the same normalised response to a class the user
#    set at 6 km/h assumes the gradient response scales proportionally down to a
#    third of the observed flat speed. Nothing in the literature tests this.
# 2. Nothing here describes a loaded utility bicycle on an unpaved track, which
#    is the dominant real case for AccessMod. Expect the true uphill response to
#    be harsher and the dismount gradient lower than modelled.
# 3. factor() is continuous everywhere but not differentiable at S = 0. The
#    kink is real (climbing costs more than descending gains) and both sides are
#    measured: +3.95 descending, -6.66 climbing.
# 4. K_BRAKE_STEEP rests on one open-ended, unconditional bin. Run the
#    sensitivity bracket (4.8 to 12.8) before publishing isochrones that depend
#    on steep-descent travel.
# 5. Recommended sensitivity run for the climb limb: K_UPHILL in [5.5, 9.0].
# =============================================================================

args <- commandArgs(trailingOnly = TRUE)
check_only <- "--check" %in% args
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

if (length(script_arg) != 1L) {
  stop("Run this generator with Rscript")
}

script_path <- sub("^--file=", "", script_arg[[1L]])
script_directory <- dirname(normalizePath(script_path, mustWork = TRUE))
csv_path <- file.path(script_directory, "bicycle-lut.csv")
header_path <- file.path(script_directory, "bicycle-lut.generated.h")

# --- Model constants ---------------------------------------------------------
# value, and the source that fixes it. Anything not listed here is derived.

parameters <- data.frame(
  name = c(
    "k_uphill",
    "k_downhill",
    "slope_peak",
    "k_brake",
    "slope_brake_steep",
    "k_brake_steep",
    "hike_flat_speed",
    "hike_tobler_k",
    "hike_tobler_s0",
    "hike_min_speed",
    "evidence_min_slope",
    "evidence_max_slope",
    "table_min_slope",
    "table_max_slope",
    "table_step"
  ),
  value = c(
    6.66,    # speed lost per unit gradient, climbing
    3.95,    # speed gained per unit gradient, gentle descent
    -0.055,  # gradient of maximum speed
    3.68,    # speed lost per unit gradient, -5.5% to -10%
    -0.10,   # end of the Flugel observed classes
    7.60,    # speed lost per unit gradient beyond -10%
    3.2,     # km/h, pushing the bicycle on the flat
    3.5,     # Tobler decay constant
    0.05,    # Tobler offset (walking optimum is a gentle descent)
    0.1,     # km/h, floor of the push branch
    -0.10,   # steepest descent with observed support
    0.09,    # steepest climb with observed support
    -0.40,
    0.20,
    0.005
  ),
  source = c(
    "Parkin2010",       # 40.02 / 6.01
    "Parkin2010+Flugel2019",  # 3.96 and 3.95 independently
    "Flugel2019",
    "Flugel2019",
    "Flugel2019",
    "Maurer2025_digitised",
    "AccessMod",
    "Tobler1993",
    "Tobler1993",
    "AccessMod",
    "Flugel2019",
    "Parkin2010",
    "AccessMod",
    "AccessMod",
    "AccessMod"
  ),
  stringsAsFactors = FALSE
)

get_parameter <- function(name) {
  parameters$value[[match(name, parameters$name)]]
}

K_UPHILL <- get_parameter("k_uphill")
K_DOWNHILL <- get_parameter("k_downhill")
SLOPE_PEAK <- get_parameter("slope_peak")
K_BRAKE <- get_parameter("k_brake")
SLOPE_BRAKE_STEEP <- get_parameter("slope_brake_steep")
K_BRAKE_STEEP <- get_parameter("k_brake_steep")
TABLE_MIN <- get_parameter("table_min_slope")
TABLE_MAX <- get_parameter("table_max_slope")
TABLE_STEP <- get_parameter("table_step")

# Derived, not chosen.
FACTOR_PEAK <- 1 + K_DOWNHILL * abs(SLOPE_PEAK)
FACTOR_BRAKE_STEEP <- FACTOR_PEAK -
  K_BRAKE * (abs(SLOPE_BRAKE_STEEP) - abs(SLOPE_PEAK))

speed_factor <- function(slope) {
  vapply(slope, function(s) {
    if (s >= 0) {
      return(max(0, 1 - K_UPHILL * s))
    }
    steepness <- abs(s)
    if (s >= SLOPE_PEAK) {
      return(1 + K_DOWNHILL * steepness)
    }
    if (s >= SLOPE_BRAKE_STEEP) {
      return(FACTOR_PEAK - K_BRAKE * (steepness - abs(SLOPE_PEAK)))
    }
    max(0, FACTOR_BRAKE_STEEP -
      K_BRAKE_STEEP * (steepness - abs(SLOPE_BRAKE_STEEP)))
  }, numeric(1L))
}

# --- Observations retained for validation ------------------------------------
# Flugel exp(beta) class means. The -9% and -8% entries share a coefficient
# because both fall in the published -9% to -7% class; the -10% entry is the
# open-ended "below -9%" class and is treated as a point value.

flugel_slope <- seq(-0.10, 0.00, by = 0.01)
flugel_beta <- c(
  0.0491, 0.1081, 0.1081, 0.1357, 0.1795, 0.1802,
  0.1494, 0.1124, 0.0589, 0.0412, 0.0000
)
flugel_factor <- exp(flugel_beta)

# Maurer digitised medians. Open-ended classes carry NA for the bin centre and
# are excluded from the residual check.

maurer_centre <- c(NA, -0.08, -0.04, 0.00, 0.04, 0.08, NA)
maurer_median <- c(14.11, 29.70, 26.46, 21.10, 13.37, 10.25, 6.13)
maurer_label <- c(
  "below_-10pct", "-10_to_-6pct", "-6_to_-2pct", "-2_to_2pct",
  "2_to_6pct", "6_to_10pct", "above_10pct"
)
maurer_factor <- maurer_median / maurer_median[[4L]]

# --- Fit quality against Flugel ----------------------------------------------

flugel_residual <- flugel_factor - speed_factor(flugel_slope)

message(sprintf(
  "Flugel fit: mean|resid| = %.4f, max = %.4f, RMSE = %.4f (n = %d)",
  mean(abs(flugel_residual)),
  max(abs(flugel_residual)),
  sqrt(mean(flugel_residual^2)),
  length(flugel_residual)
))

if (max(abs(flugel_residual)) > 0.05) {
  stop("Piecewise-linear model no longer reproduces the Flugel class means")
}

# --- CSV ---------------------------------------------------------------------

# Rounded so that breakpoints land exactly on table nodes and so that --check
# is byte-stable regardless of platform floating-point accumulation in seq().
table_slope <- round(seq(TABLE_MIN, TABLE_MAX, by = TABLE_STEP), 3L)
table_factor <- speed_factor(table_slope)

csv_lines <- c(
  "kind,name,slope,value,residual,source",
  vapply(seq_len(nrow(parameters)), function(i) {
    sprintf(
      "parameter,%s,,%.12f,,%s",
      parameters$name[[i]], parameters$value[[i]], parameters$source[[i]]
    )
  }, character(1L)),
  sprintf("derived,factor_peak,%.12f,%.12f,,derived", SLOPE_PEAK, FACTOR_PEAK),
  sprintf(
    "derived,factor_at_brake_steep,%.12f,%.12f,,derived",
    SLOPE_BRAKE_STEEP, FACTOR_BRAKE_STEEP
  ),
  vapply(seq_along(table_slope), function(i) {
    provenance <- if (table_slope[[i]] >= 0) {
      if (table_slope[[i]] <= get_parameter("evidence_max_slope")) {
        "Parkin2010"
      } else {
        "Parkin2010_extrapolated"
      }
    } else if (table_slope[[i]] >= SLOPE_BRAKE_STEEP) {
      "Flugel2019"
    } else {
      "Maurer2025_digitised"
    }
    sprintf(
      "speed_factor,,%.12f,%.12f,,%s",
      table_slope[[i]], table_factor[[i]], provenance
    )
  }, character(1L)),
  vapply(seq_along(flugel_slope), function(i) {
    sprintf(
      "observation_flugel,,%.12f,%.12f,%.12f,Flugel2019",
      flugel_slope[[i]], flugel_factor[[i]], flugel_residual[[i]]
    )
  }, character(1L)),
  vapply(seq_along(maurer_median), function(i) {
    centre <- if (is.na(maurer_centre[[i]])) {
      ""
    } else {
      sprintf("%.12f", maurer_centre[[i]])
    }
    sprintf(
      "observation_maurer,%s,%s,%.12f,,Maurer2025_digitised",
      maurer_label[[i]], centre, maurer_factor[[i]]
    )
  }, character(1L))
)

# --- Header ------------------------------------------------------------------

format_factor_rows <- function(slopes, factors) {
  rows <- vapply(seq_along(slopes), function(i) {
    sprintf("    {%+.3f, %.12f}", slopes[[i]], factors[[i]])
  }, character(1L))

  paste0(rows, c(rep(",", length(rows) - 1L), ""))
}

header_lines <- c(
  "/* Generated by generate-bicycle-lut.R. Do not edit manually. */",
  "/*",
  " * Bicycle speed response to gradient, normalised to the user-supplied flat",
  " * speed of the landcover class:  v(S) = V_flat * factor(S), floored by the",
  " * speed of pushing the bicycle.",
  " *",
  " * Climb limb   Parkin & Rotheram 2010, Transport Policy 17(5) 335-341.",
  " * Descent limb Flugel et al. 2019, Transportation 46 1395-1419.",
  " * Steep tail   Maurer et al. 2025, J Cycl Micromobility Res 5 100077,",
  " *              digitised from the published boxplot (data is confidential).",
  " * Push branch  Tobler 1993, NCGIA Technical Report 93-1.",
  " *",
  " * There is no dismount threshold. Riding stops where the ride branch falls",
  " * below the push branch, which depends on V_flat. Callers must therefore",
  " * evaluate both branches and take the maximum:",
  " *",
  " *   ride = V_flat * am_bicycle_speed_factor(S)",
  " *   push = fmin(V_flat, AM_BICYCLE_HIKE_FLAT_SPEED)",
  " *          * exp(-AM_BICYCLE_HIKE_TOBLER_K * fabs(S + AM_BICYCLE_HIKE_TOBLER_S0))",
  " *          / exp(-AM_BICYCLE_HIKE_TOBLER_K * AM_BICYCLE_HIKE_TOBLER_S0)",
  " *   v    = fmax(fmax(ride, push), AM_BICYCLE_HIKE_MIN_SPEED)",
  " *",
  " * Outside [AM_BICYCLE_SLOPE_MIN, AM_BICYCLE_SLOPE_MAX] the factor is zero and",
  " * the push branch governs. See bicycle-lut.csv for per-row provenance and for",
  " * the observations the table was validated against.",
  " */",
  "#ifndef ACCESSMOD_BICYCLE_LUT_GENERATED_H",
  "#define ACCESSMOD_BICYCLE_LUT_GENERATED_H",
  "",
  sprintf("#define AM_BICYCLE_HIKE_FLAT_SPEED (%.1f)", get_parameter("hike_flat_speed")),
  sprintf("#define AM_BICYCLE_HIKE_MIN_SPEED (%.1f)", get_parameter("hike_min_speed")),
  sprintf("#define AM_BICYCLE_HIKE_TOBLER_K (%.1f)", get_parameter("hike_tobler_k")),
  sprintf("#define AM_BICYCLE_HIKE_TOBLER_S0 (%.2f)", get_parameter("hike_tobler_s0")),
  "",
  sprintf("#define AM_BICYCLE_SLOPE_MIN (%.3f)", TABLE_MIN),
  sprintf("#define AM_BICYCLE_SLOPE_MAX (%.3f)", TABLE_MAX),
  sprintf("#define AM_BICYCLE_SLOPE_STEP (%.3f)", TABLE_STEP),
  sprintf("#define AM_BICYCLE_SLOPE_COUNT (%d)", length(table_slope)),
  "",
  "/* Slopes with direct observational support; outside this the table is an",
  " * extrapolation and should be reported as such. */",
  sprintf("#define AM_BICYCLE_EVIDENCE_MIN (%.2f)", get_parameter("evidence_min_slope")),
  sprintf("#define AM_BICYCLE_EVIDENCE_MAX (%.2f)", get_parameter("evidence_max_slope")),
  "",
  "static const struct am_speed_factor am_bicycle_speed_factors[] = {",
  format_factor_rows(table_slope, table_factor),
  "};",
  "",
  "#endif /* ACCESSMOD_BICYCLE_LUT_GENERATED_H */"
)

# --- Emit --------------------------------------------------------------------

sync_file <- function(path, expected_lines) {
  expected_lines <- unname(expected_lines)

  if (check_only) {
    if (!file.exists(path) || !identical(readLines(path, warn = FALSE), expected_lines)) {
      stop(basename(path), " is stale; run generate-bicycle-lut.R")
    }
    return(invisible(NULL))
  }

  writeLines(expected_lines, path, useBytes = TRUE)
  message("Wrote ", path)
}

sync_file(csv_path, csv_lines)
sync_file(header_path, header_lines)

if (check_only) {
  message("Bicycle LUT generated files are up to date")
}
