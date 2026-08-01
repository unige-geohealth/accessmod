#!/usr/bin/env Rscript

# Generate a publication-oriented diagnostic plot from the canonical bicycle
# model. The generated LUT itself is a build artefact and is not versioned.
#
# Usage:
#   Rscript plot-bicycle-lut.R [output.png]

args <- commandArgs(trailingOnly = TRUE)
script_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)

if (length(script_arg) != 1L) {
  stop("Run this plotter with Rscript")
}

script_path <- sub("^--file=", "", script_arg[[1L]])
script_directory <- dirname(normalizePath(script_path, mustWork = TRUE))
output_file <- if (length(args) >= 1L) args[[1L]] else "bicycle-lut.png"
output_directory <- dirname(normalizePath(output_file, mustWork = FALSE))
dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)

generated_directory <- tempfile("accessmod-bicycle-lut-")
dir.create(generated_directory)
on.exit(unlink(generated_directory, recursive = TRUE), add = TRUE)

status <- system2(
  "Rscript",
  c(
    file.path(script_directory, "generate-bicycle-lut.R"),
    paste0("--output-dir=", generated_directory)
  )
)
if (status != 0L) {
  stop("Bicycle LUT generation failed")
}

diagnostics <- read.csv(
  file.path(generated_directory, "bicycle-lut.csv"),
  stringsAsFactors = FALSE
)

flat_speeds <- c(6, 12, 20)
colors <- setNames(grDevices::hcl.colors(length(flat_speeds), "Dark 3"),
                   flat_speeds)
power_rows <- diagnostics$kind == "equivalent_power"
powers <- setNames(
  diagnostics$value[power_rows],
  diagnostics$flat_speed[power_rows]
)

selected <- diagnostics[diagnostics$kind == "diagnostic_selected", ]
selected <- selected[selected$flat_speed %in% flat_speeds, ]
y_max <- max(selected$value) * 1.08

grDevices::png(
  filename = output_file,
  width = 2000,
  height = 1500,
  units = "px",
  res = 180,
  bg = "white"
)

old_par <- graphics::par(no.readonly = TRUE)
on.exit({
  graphics::par(old_par)
  grDevices::dev.off()
}, add = TRUE)

graphics::par(
  mfrow = c(2, 1),
  mar = c(4.2, 4.8, 3.1, 1.2),
  oma = c(1.8, 0, 2.2, 0),
  mgp = c(2.7, 0.8, 0),
  las = 1,
  family = "sans"
)

# Panel 1: final LUT result for the three documented scenario speeds.
graphics::plot(
  NA,
  xlim = c(-30, 30),
  ylim = c(0, y_max),
  xlab = "Slope in direction of travel (%)",
  ylab = "Modelled speed (km/h)",
  main = "Physics-based bicycle speed with downhill safety envelope",
  xaxs = "i",
  yaxs = "i"
)
graphics::rect(-30, 0, 0, y_max, col = "#f2f6fa", border = NA)
graphics::rect(0, 0, 30, y_max, col = "#f4f8f2", border = NA)
graphics::abline(v = 100 * c(-0.25, -0.10, -0.055, 0),
                 lty = c(3, 2, 2, 1), col = "#777777")
graphics::grid(col = "white", lty = 1)

for (flat_speed in flat_speeds) {
  rows <- selected[selected$flat_speed == flat_speed, ]
  graphics::lines(
    rows$slope * 100,
    rows$value,
    col = colors[[as.character(flat_speed)]],
    lwd = 2.8
  )
}

graphics::legend(
  "topright",
  legend = vapply(flat_speeds, function(flat_speed) {
    sprintf(
      "%g km/h flat (%.0f W equivalent)",
      flat_speed, powers[[as.character(flat_speed)]]
    )
  }, character(1L)),
  col = colors,
  lwd = 2.8,
  bg = grDevices::adjustcolor("white", alpha.f = 0.9),
  inset = 0.015,
  cex = 0.80
)

# Panel 2: branch decomposition makes every modelling decision visible.
reference_speed <- 12
component_kinds <- c(
  physics = "diagnostic_physics",
  envelope = "diagnostic_safety_envelope",
  ride = "diagnostic_ride",
  hike = "diagnostic_hike",
  selected = "diagnostic_selected"
)
component_colors <- c(
  physics = "#756bb1",
  envelope = "#d7301f",
  ride = "#2171b5",
  hike = "#636363",
  selected = "#238b45"
)
component_lty <- c(physics = 3, envelope = 2, ride = 4, hike = 5, selected = 1)

reference_rows <- diagnostics[
  !is.na(diagnostics$flat_speed) & diagnostics$flat_speed == reference_speed,
]
reference_selected <- reference_rows[
  reference_rows$kind == component_kinds[["selected"]],
]
reference_y_max <- max(reference_selected$value) * 1.15

graphics::plot(
  NA,
  xlim = c(-30, 30),
  ylim = c(0, reference_y_max),
  xlab = "Slope in direction of travel (%)",
  ylab = "Speed (km/h)",
  main = "Model branches at 12 km/h flat speed",
  xaxs = "i",
  yaxs = "i"
)
graphics::grid(col = "#eeeeee", lty = 1)
graphics::abline(v = 100 * c(-0.25, -0.10, -0.055, 0),
                 lty = c(3, 2, 2, 1), col = "#bbbbbb")

for (name in names(component_kinds)) {
  rows <- reference_rows[reference_rows$kind == component_kinds[[name]], ]
  graphics::lines(
    rows$slope * 100,
    rows$value,
    col = component_colors[[name]],
    lty = component_lty[[name]],
    lwd = if (name == "selected") 3.2 else 2.0
  )
}

graphics::legend(
  "topright",
  legend = c("Physics", "Safety envelope", "Ride", "Hike-a-bike", "Selected"),
  col = component_colors,
  lty = component_lty,
  lwd = c(2, 2, 2, 2, 3.2),
  bg = grDevices::adjustcolor("white", alpha.f = 0.9),
  inset = 0.015,
  cex = 0.80
)

graphics::mtext(
  "AccessMod bicycle mobility model — generated from the same canonical parameters as the C LUT",
  side = 3,
  outer = TRUE,
  line = 0.5,
  cex = 1.0,
  font = 2
)
