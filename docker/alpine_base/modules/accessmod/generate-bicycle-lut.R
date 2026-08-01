#!/usr/bin/env Rscript

# Generate the AccessMod bicycle speed LUT used by mobility.h.
#
# The scientific model lives here. Runtime C code only performs bilinear
# interpolation over the generated slope x flat-speed table.

args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

if (length(script_arg) != 1L) {
  stop("Run this generator with Rscript")
}

script_path <- sub("^--file=", "", script_arg[[1L]])
script_directory <- dirname(normalizePath(script_path, mustWork = TRUE))
output_arg <- grep("^--output-dir=", args, value = TRUE)
output_directory <- if (length(output_arg) == 1L) {
  sub("^--output-dir=", "", output_arg[[1L]])
} else {
  script_directory
}
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

header_path <- file.path(output_directory, "bicycle-lut.generated.h")
csv_path <- file.path(output_directory, "bicycle-lut.csv")

# --- Canonical parameters ----------------------------------------------------

parameters <- c(
  rider_mass_kg = 80.0,
  bicycle_mass_kg = 15.0,
  gravity_m_s2 = 9.8,
  rolling_resistance = 0.012,
  frontal_area_m2 = 0.445,
  wind_speed_m_s = 0.0,
  temperature_c = 20.0,
  elevation_m = 500.0,
  transmission_efficiency = 0.90,
  hike_flat_speed_km_h = 3.2,
  hike_min_speed_km_h = 0.1
)

envelope_slope <- c(-0.25, -0.10, -0.055, 0.00)
envelope_factor <- c(0.00, 1.05, 1.20, 1.00)

speed_min <- 0.0
speed_max <- 100.0
speed_step <- 0.5
slope_min <- -1.20
slope_max <- 1.00
slope_step <- 0.005

flat_speeds <- seq(speed_min, speed_max, by = speed_step)
slopes <- seq(slope_min, slope_max, by = slope_step)

air_density <-
  (1.293 - 0.00426 * parameters[["temperature_c"]]) *
  exp(-parameters[["elevation_m"]] / 7000.0)
weight_force <-
  parameters[["gravity_m_s2"]] *
  (parameters[["rider_mass_kg"]] + parameters[["bicycle_mass_kg"]])
air_resistance <- 0.5 * parameters[["frontal_area_m2"]] * air_density

# --- Exact model used to compile the LUT ------------------------------------

equivalent_power <- function(flat_speed) {
  velocity <- flat_speed / 3.6
  resistance <- weight_force * parameters[["rolling_resistance"]]
  air_velocity <- velocity + parameters[["wind_speed_m_s"]]

  (
    velocity * resistance + velocity * air_velocity^2 * air_resistance
  ) / parameters[["transmission_efficiency"]]
}

physics_speed <- function(flat_speed, slope) {
  if (flat_speed <= 0) {
    return(0)
  }

  power <- equivalent_power(flat_speed)
  resistance <-
    weight_force * (slope + parameters[["rolling_resistance"]])

  residual <- function(velocity) {
    air_velocity <- velocity + parameters[["wind_speed_m_s"]]
    velocity * (air_resistance * air_velocity^2 + resistance) -
      parameters[["transmission_efficiency"]] * power
  }

  upper <- max(flat_speed / 3.6, 1.0)
  while (residual(upper) < 0) {
    upper <- upper * 2
  }

  uniroot(residual, c(0, upper), tol = 1e-11)$root * 3.6
}

safety_factor <- function(slope) {
  if (slope >= 0) {
    return(Inf)
  }
  if (slope <= envelope_slope[[1L]]) {
    return(envelope_factor[[1L]])
  }

  approx(
    envelope_slope,
    envelope_factor,
    xout = slope,
    ties = "ordered"
  )$y
}

hike_speed <- function(flat_speed, slope) {
  if (flat_speed <= 0) {
    return(0)
  }

  flat_hike <- min(flat_speed, parameters[["hike_flat_speed_km_h"]])
  minimum <- min(flat_speed, parameters[["hike_min_speed_km_h"]])

  max(minimum, flat_hike * exp(0.175 - 3.5 * abs(slope + 0.05)))
}

model_components <- function(flat_speed, slope) {
  if (flat_speed <= 0) {
    return(c(physics = 0, safety_envelope = 0, ride = 0, hike = 0,
             selected = 0))
  }

  physics <- physics_speed(flat_speed, slope)
  envelope <- if (slope < 0) flat_speed * safety_factor(slope) else Inf
  ride <- min(physics, envelope)
  hike <- hike_speed(flat_speed, slope)

  c(
    physics = physics,
    safety_envelope = envelope,
    ride = ride,
    hike = hike,
    selected = max(ride, hike)
  )
}

model_speed <- function(flat_speed, slope) {
  model_components(flat_speed, slope)[["selected"]]
}

message(sprintf(
  "Generating %d x %d bicycle LUT (%d cells)",
  length(flat_speeds), length(slopes),
  length(flat_speeds) * length(slopes)
))

lut <- outer(flat_speeds, slopes, Vectorize(model_speed))

# --- Approximation check -----------------------------------------------------

lut_interpolate <- function(flat_speed, slope) {
  flat_speed <- max(speed_min, min(speed_max, flat_speed))
  slope <- max(slope_min, min(slope_max, slope))

  speed_position <- (flat_speed - speed_min) / speed_step
  slope_position <- (slope - slope_min) / slope_step
  speed_index <- min(length(flat_speeds) - 1L, floor(speed_position) + 1L)
  slope_index <- min(length(slopes) - 1L, floor(slope_position) + 1L)
  speed_fraction <- speed_position - floor(speed_position)
  slope_fraction <- slope_position - floor(slope_position)

  lower <-
    lut[speed_index, slope_index] * (1 - speed_fraction) +
    lut[speed_index + 1L, slope_index] * speed_fraction
  upper <-
    lut[speed_index, slope_index + 1L] * (1 - speed_fraction) +
    lut[speed_index + 1L, slope_index + 1L] * speed_fraction

  lower * (1 - slope_fraction) + upper * slope_fraction
}

# Cell centres cover every interpolation cell in the operational 1-40 km/h
# range and catch the branch-crossing errors that dominate the approximation.
validation_speeds <- seq(1.25, 39.75, by = speed_step)
validation_slopes <- seq(slope_min + slope_step / 2,
                         slope_max - slope_step / 2,
                         by = slope_step)
validation_error <- numeric(length(validation_speeds) * length(validation_slopes))
error_index <- 1L

for (flat_speed in validation_speeds) {
  for (slope in validation_slopes) {
    validation_error[[error_index]] <- abs(
      model_speed(flat_speed, slope) - lut_interpolate(flat_speed, slope)
    )
    error_index <- error_index + 1L
  }
}

max_error <- max(validation_error)
p99_error <- unname(quantile(validation_error, 0.99))
message(sprintf(
  "Interpolation error (1-40 km/h): max %.4f km/h, p99 %.4f km/h",
  max_error, p99_error
))

if (max_error > 0.35 || p99_error > 0.05) {
  stop("Bicycle LUT interpolation error exceeds its acceptance limits")
}

# --- C header ---------------------------------------------------------------

format_lut_row <- function(values) {
  literals <- sprintf("%.8ff", values)
  groups <- split(literals, ceiling(seq_along(literals) / 8L))
  lines <- vapply(groups, function(group) {
    paste0("        ", paste(group, collapse = ", "))
  }, character(1L))
  paste(lines, collapse = ",\n")
}

lut_rows <- vapply(seq_along(flat_speeds), function(i) {
  paste0("    {\n", format_lut_row(lut[i, ]), "\n    }")
}, character(1L))

header_lines <- c(
  "/* Generated by generate-bicycle-lut.R. Do not edit manually. */",
  "#ifndef ACCESSMOD_BICYCLE_LUT_GENERATED_H",
  "#define ACCESSMOD_BICYCLE_LUT_GENERATED_H",
  "",
  sprintf("#define AM_BICYCLE_SPEED_MIN (%.1f)", speed_min),
  sprintf("#define AM_BICYCLE_SPEED_MAX (%.1f)", speed_max),
  sprintf("#define AM_BICYCLE_SPEED_STEP (%.1f)", speed_step),
  sprintf("#define AM_BICYCLE_SPEED_COUNT (%d)", length(flat_speeds)),
  sprintf("#define AM_BICYCLE_SLOPE_MIN (%.3f)", slope_min),
  sprintf("#define AM_BICYCLE_SLOPE_MAX (%.3f)", slope_max),
  sprintf("#define AM_BICYCLE_SLOPE_STEP (%.3f)", slope_step),
  sprintf("#define AM_BICYCLE_SLOPE_COUNT (%d)", length(slopes)),
  "",
  "static const float",
  "am_bicycle_speed_lut[AM_BICYCLE_SPEED_COUNT][AM_BICYCLE_SLOPE_COUNT] = {",
  paste0(lut_rows, c(rep(",", length(lut_rows) - 1L), "")),
  "};",
  "",
  "#endif /* ACCESSMOD_BICYCLE_LUT_GENERATED_H */"
)

writeLines(header_lines, header_path, useBytes = TRUE)

# --- Human-readable diagnostics ---------------------------------------------

csv_lines <- c(
  "kind,name,flat_speed,slope,value,source",
  vapply(names(parameters), function(name) {
    sprintf("parameter,%s,,,%.12f,AccessMod", name, parameters[[name]])
  }, character(1L)),
  sprintf("grid,speed_min,,,%.12f,AccessMod", speed_min),
  sprintf("grid,speed_max,,,%.12f,AccessMod", speed_max),
  sprintf("grid,speed_step,,,%.12f,AccessMod", speed_step),
  sprintf("grid,slope_min,,,%.12f,AccessMod", slope_min),
  sprintf("grid,slope_max,,,%.12f,AccessMod", slope_max),
  sprintf("grid,slope_step,,,%.12f,AccessMod", slope_step),
  vapply(seq_along(envelope_slope), function(i) {
    source <- if (i %in% c(2L, 3L)) "Flugel2019" else "AccessMod"
    sprintf(
      "safety_anchor,,,%+.12f,%.12f,%s",
      envelope_slope[[i]], envelope_factor[[i]], source
    )
  }, character(1L)),
  vapply(c(6, 12, 18, 20, 22), function(flat_speed) {
    sprintf(
      "equivalent_power,,%.12f,,%.12f,physical_model",
      flat_speed, equivalent_power(flat_speed)
    )
  }, character(1L))
)

diagnostic_slopes <- seq(-0.30, 0.30, by = slope_step)
for (flat_speed in c(6, 12, 20)) {
  for (slope in diagnostic_slopes) {
    components <- model_components(flat_speed, slope)
    for (name in names(components)) {
      value <- components[[name]]
      if (is.infinite(value)) {
        next
      }
      csv_lines <- c(csv_lines, sprintf(
        "diagnostic_%s,,%.12f,%+.12f,%.12f,derived",
        name, flat_speed, slope, value
      ))
    }
  }
}

writeLines(csv_lines, csv_path, useBytes = TRUE)
message("Wrote ", header_path)
message("Wrote ", csv_path)
