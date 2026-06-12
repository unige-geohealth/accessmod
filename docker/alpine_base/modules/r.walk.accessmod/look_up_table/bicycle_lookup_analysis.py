#!/usr/bin/env python3
"""Explore bicycle lookup-table approximations for r.walk.accessmod.

This script ports the current C bicycle model closely enough for research:

- `bicycle_speed_raw()` mirrors `bicycleSpeed()` from `bicycleSpeed.h`.
- `current_speed_capped()` mirrors the `speedBicycle()` wrapper from
  `timeCostManager.h`, including the `2 * flat_speed` downhill cap.
- `hybrid_routing_speed()` is a draft hybrid curve: physics where riding is
  plausible, Tobler-style walking where the cyclist is assumed to dismount.

It then builds a lookup table, interpolates between samples, and reports
approximation error against the hybrid curve. The current model is kept as a
comparison because its steep-downhill cap creates an artificial plateau and
discontinuity.
SVG plots are generated with only the Python standard library.
"""

from __future__ import annotations

import argparse
import csv
import math
from dataclasses import dataclass
from pathlib import Path
from statistics import mean
from typing import Callable, Iterable


MAX_RATIO = 2.0
SLOPE_FLAT = 0.0
WEIGHT_RIDER = 80.0
WEIGHT_BICYCLE = 15.0
RESISTANCE_ROLLING = 0.012
AREA_FRONTAL = 0.445
SPEED_WIND = 0.0
TEMPERATURE = 20.0
ELEVATION = 500.0
EFFICIENCY_TRANSMISSION = 0.90


@dataclass(frozen=True)
class RoutingCurveParams:
    downhill_blocked_slope: float = -25.0
    downhill_walk_slope: float = -18.0
    downhill_physics_slope: float = -5.0
    uphill_physics_slope: float = 20.0
    uphill_walk_slope: float = 25.0
    uphill_blocked_slope: float = 35.0
    blocked_speed: float = 0.25
    walk_bike_flat_speed: float = 3.5


@dataclass(frozen=True)
class LookupTable:
    slope_min: float
    slope_max: float
    values: tuple[float, ...]

    @property
    def count(self) -> int:
        return len(self.values)

    @property
    def step(self) -> float:
        if self.count <= 1:
            return 0.0
        return (self.slope_max - self.slope_min) / (self.count - 1)


def newton(aero: float, hw: float, tr: float, tran: float, power: float) -> float:
    """Port of the C Newton solver in bicycleSpeed.h."""

    max_iter = 10
    vel = 20.0
    tol = 0.05

    for _ in range(1, max_iter):
        tv = vel + hw
        f = vel * (aero * tv * tv + tr) - tran * power
        fp = aero * (3.0 * vel + hw) * tv + tr
        if fp == 0.0:
            return 0.0
        v_new = vel - f / fp
        if abs(v_new - vel) < tol:
            return v_new
        vel = v_new

    return 0.0


def bicycle_speed_raw(flat_speed: float, slope_percent: float) -> float:
    """Port of bicycleSpeed(speed, slope), without wrapper caps."""

    density_air = (1.293 - 0.00426 * TEMPERATURE) * math.exp(-ELEVATION / 7000.0)
    weight_total = 9.8 * (WEIGHT_RIDER + WEIGHT_BICYCLE)
    resistance_air = 0.5 * AREA_FRONTAL * density_air

    speed_bike = flat_speed / 3.6
    slope_out = slope_percent * 0.01

    resistance_slope_tire_flat = weight_total * (SLOPE_FLAT + RESISTANCE_ROLLING)
    resistance_slope_tire_up = weight_total * (slope_out + RESISTANCE_ROLLING)
    speed_total = speed_bike + SPEED_WIND
    power_flat = (
        speed_bike * resistance_slope_tire_flat
        + speed_bike * speed_total * speed_total * resistance_air
    ) / EFFICIENCY_TRANSMISSION

    return (
        newton(
            resistance_air,
            SPEED_WIND,
            resistance_slope_tire_up,
            EFFICIENCY_TRANSMISSION,
            power_flat,
        )
        * 3.6
    )


def current_speed_capped(flat_speed: float, slope_percent: float) -> float:
    """Port of speedBicycle(speed, slope) used by costManager()."""

    speed_final = bicycle_speed_raw(flat_speed, slope_percent)
    if speed_final >= flat_speed * MAX_RATIO:
        speed_final = flat_speed * MAX_RATIO
    if speed_final < 0.0:
        speed_final = 0.0
    return speed_final


def smoothstep(edge0: float, edge1: float, value: float) -> float:
    if edge0 == edge1:
        return 1.0 if value >= edge1 else 0.0
    x = min(1.0, max(0.0, (value - edge0) / (edge1 - edge0)))
    return x * x * (3.0 - 2.0 * x)


def smooth_lerp(start: float, stop: float, ratio: float) -> float:
    return start + (stop - start) * smoothstep(0.0, 1.0, ratio)


def interpolate_anchors(
    value: float,
    anchors: list[tuple[float, float]],
) -> float:
    if value <= anchors[0][0]:
        return anchors[0][1]
    if value >= anchors[-1][0]:
        return anchors[-1][1]

    for index in range(len(anchors) - 1):
        x0, y0 = anchors[index]
        x1, y1 = anchors[index + 1]
        if x0 <= value <= x1:
            ratio = (value - x0) / (x1 - x0)
            return smooth_lerp(y0, y1, ratio)

    return anchors[-1][1]


def physics_riding_speed(flat_speed: float, slope_percent: float) -> float:
    """Physics-based bicycle speed used in the rideable range.

    This uses the current AccessMod wrapper, including the existing downhill
    `2 * flat_speed` cap. The hybrid model only trusts it in the slope range
    where cycling remains operationally plausible.
    """

    return current_speed_capped(flat_speed, slope_percent)


def tobler_walk_speed(flat_speed: float, slope_percent: float) -> float:
    """Tobler-style walking speed for pushing a bicycle.

    Tobler's formula uses slope as a gradient ratio, while this analysis uses
    percent slope everywhere else.
    """

    slope_ratio = slope_percent / 100.0
    top_speed = flat_speed / math.exp(-0.175)
    return math.exp(-3.5 * abs(slope_ratio + 0.05)) * top_speed


def walk_bike_speed(slope_percent: float, params: RoutingCurveParams) -> float:
    return tobler_walk_speed(params.walk_bike_flat_speed, slope_percent)


def hybrid_routing_speed(
    flat_speed: float,
    slope_percent: float,
    params: RoutingCurveParams,
) -> float:
    """Draft hybrid bicycle routing speed.

    The curve keeps the physics-based riding model from -5% to +20%, then
    blends toward Tobler walking speed where the cyclist is assumed to dismount.
    The steep-slope thresholds are proposed defaults for sensitivity testing.
    """

    p = params
    physics = lambda slope: physics_riding_speed(flat_speed, slope)
    walk = lambda slope: walk_bike_speed(slope, p)

    if slope_percent <= p.downhill_blocked_slope:
        return p.blocked_speed
    if slope_percent < p.downhill_walk_slope:
        ratio = (slope_percent - p.downhill_blocked_slope) / (
            p.downhill_walk_slope - p.downhill_blocked_slope
        )
        return smooth_lerp(p.blocked_speed, walk(slope_percent), ratio)
    if slope_percent < p.downhill_physics_slope:
        ratio = (slope_percent - p.downhill_walk_slope) / (
            p.downhill_physics_slope - p.downhill_walk_slope
        )
        return smooth_lerp(walk(slope_percent), physics(slope_percent), ratio)
    if slope_percent <= p.uphill_physics_slope:
        return physics(slope_percent)
    if slope_percent < p.uphill_walk_slope:
        ratio = (slope_percent - p.uphill_physics_slope) / (
            p.uphill_walk_slope - p.uphill_physics_slope
        )
        return smooth_lerp(physics(slope_percent), walk(slope_percent), ratio)
    if slope_percent < p.uphill_blocked_slope:
        ratio = (slope_percent - p.uphill_walk_slope) / (
            p.uphill_blocked_slope - p.uphill_walk_slope
        )
        return smooth_lerp(walk(slope_percent), p.blocked_speed, ratio)
    return p.blocked_speed


def linspace(start: float, stop: float, count: int) -> list[float]:
    if count < 2:
        raise ValueError("count must be >= 2")
    step = (stop - start) / (count - 1)
    return [start + i * step for i in range(count)]


def build_lookup(
    flat_speed: float,
    slope_min: float,
    slope_max: float,
    count: int,
    model: Callable[[float, float], float],
) -> LookupTable:
    slopes = linspace(slope_min, slope_max, count)
    values = tuple(model(flat_speed, slope) for slope in slopes)
    return LookupTable(slope_min=slope_min, slope_max=slope_max, values=values)


def lookup_speed(table: LookupTable, slope_percent: float) -> float:
    if slope_percent <= table.slope_min:
        return table.values[0]
    if slope_percent >= table.slope_max:
        return table.values[-1]

    pos = (slope_percent - table.slope_min) / table.step
    idx = int(math.floor(pos))
    ratio = pos - idx
    left = table.values[idx]
    right = table.values[idx + 1]
    return left + (right - left) * ratio


def percentile(values: list[float], pct: float) -> float:
    if not values:
        return 0.0
    ordered = sorted(values)
    pos = (len(ordered) - 1) * pct
    lo = int(math.floor(pos))
    hi = int(math.ceil(pos))
    if lo == hi:
        return ordered[lo]
    ratio = pos - lo
    return ordered[lo] + (ordered[hi] - ordered[lo]) * ratio


def error_stats(
    flat_speed: float,
    table: LookupTable,
    eval_slopes: Iterable[float],
    model: Callable[[float, float], float],
) -> dict[str, float]:
    abs_errors: list[float] = []
    rel_errors: list[float] = []
    max_error_slope = 0.0
    max_error = -1.0

    for slope in eval_slopes:
        expected = model(flat_speed, slope)
        observed = lookup_speed(table, slope)
        abs_error = abs(observed - expected)
        abs_errors.append(abs_error)
        if expected > 1e-9:
            rel_errors.append(abs_error / expected)
        if abs_error > max_error:
            max_error = abs_error
            max_error_slope = slope

    return {
        "max_abs_error_kmh": max(abs_errors),
        "mean_abs_error_kmh": mean(abs_errors),
        "p95_abs_error_kmh": percentile(abs_errors, 0.95),
        "max_relative_error_pct": max(rel_errors) * 100.0 if rel_errors else 0.0,
        "mean_relative_error_pct": mean(rel_errors) * 100.0 if rel_errors else 0.0,
        "max_error_slope_pct": max_error_slope,
    }


def write_stats_csv(
    path: Path,
    flat_speeds: list[float],
    sample_counts: list[int],
    slope_min: float,
    slope_max: float,
    eval_count: int,
    model: Callable[[float, float], float],
) -> None:
    eval_slopes = linspace(slope_min, slope_max, eval_count)
    fieldnames = [
        "flat_speed_kmh",
        "slope_min_pct",
        "slope_max_pct",
        "lookup_points",
        "lookup_step_pct",
        "eval_points",
        "max_abs_error_kmh",
        "mean_abs_error_kmh",
        "p95_abs_error_kmh",
        "max_relative_error_pct",
        "mean_relative_error_pct",
        "max_error_slope_pct",
    ]

    with path.open("w", newline="") as fp:
        writer = csv.DictWriter(fp, fieldnames=fieldnames)
        writer.writeheader()
        for flat_speed in flat_speeds:
            for count in sample_counts:
                table = build_lookup(flat_speed, slope_min, slope_max, count, model)
                stats = error_stats(flat_speed, table, eval_slopes, model)
                writer.writerow(
                    {
                        "flat_speed_kmh": format_num(flat_speed),
                        "slope_min_pct": format_num(slope_min),
                        "slope_max_pct": format_num(slope_max),
                        "lookup_points": count,
                        "lookup_step_pct": format_num(table.step),
                        "eval_points": eval_count,
                        **{key: format_num(value) for key, value in stats.items()},
                    }
                )


def write_model_csv(
    path: Path,
    flat_speed: float,
    slopes: list[float],
    params: RoutingCurveParams,
) -> None:
    with path.open("w", newline="") as fp:
        writer = csv.DictWriter(
            fp,
            fieldnames=[
                "slope_pct",
                "raw_speed_kmh",
                "current_speed_kmh",
                "walk_bike_speed_kmh",
                "hybrid_routing_speed_kmh",
            ],
        )
        writer.writeheader()
        for slope in slopes:
            writer.writerow(
                {
                    "slope_pct": format_num(slope),
                    "raw_speed_kmh": format_num(bicycle_speed_raw(flat_speed, slope)),
                    "current_speed_kmh": format_num(
                        current_speed_capped(flat_speed, slope)
                    ),
                    "walk_bike_speed_kmh": format_num(
                        walk_bike_speed(slope, params)
                    ),
                    "hybrid_routing_speed_kmh": format_num(
                        hybrid_routing_speed(flat_speed, slope, params)
                    ),
                }
            )


def ranges_for_predicate(
    slopes: list[float],
    predicate: Callable[[float], bool],
) -> str:
    ranges: list[tuple[float, float]] = []
    start: float | None = None
    previous = slopes[0]

    for slope in slopes:
        if predicate(slope):
            if start is None:
                start = slope
        elif start is not None:
            ranges.append((start, previous))
            start = None
        previous = slope

    if start is not None:
        ranges.append((start, slopes[-1]))

    if not ranges:
        return ""
    return "; ".join(f"{format_num(start)}..{format_num(stop)}" for start, stop in ranges)


def write_current_model_diagnostics_csv(
    path: Path,
    flat_speeds: list[float],
    slopes: list[float],
) -> None:
    with path.open("w", newline="") as fp:
        writer = csv.DictWriter(
            fp,
            fieldnames=[
                "flat_speed_kmh",
                "slope_min_pct",
                "slope_max_pct",
                "eval_points",
                "min_current_speed_kmh",
                "max_current_speed_kmh",
                "max_adjacent_jump_kmh",
                "max_adjacent_jump_from_slope_pct",
                "max_adjacent_jump_to_slope_pct",
                "zero_speed_slope_ranges_pct",
                "capped_speed_slope_ranges_pct",
                "raw_negative_slope_ranges_pct",
            ],
        )
        writer.writeheader()

        for flat_speed in flat_speeds:
            current = [current_speed_capped(flat_speed, slope) for slope in slopes]
            jumps = [
                abs(current[index + 1] - current[index])
                for index in range(len(current) - 1)
            ]
            max_jump = max(jumps) if jumps else 0.0
            max_jump_index = jumps.index(max_jump) if jumps else 0

            writer.writerow(
                {
                    "flat_speed_kmh": format_num(flat_speed),
                    "slope_min_pct": format_num(slopes[0]),
                    "slope_max_pct": format_num(slopes[-1]),
                    "eval_points": len(slopes),
                    "min_current_speed_kmh": format_num(min(current)),
                    "max_current_speed_kmh": format_num(max(current)),
                    "max_adjacent_jump_kmh": format_num(max_jump),
                    "max_adjacent_jump_from_slope_pct": format_num(
                        slopes[max_jump_index]
                    ),
                    "max_adjacent_jump_to_slope_pct": format_num(
                        slopes[max_jump_index + 1]
                    ),
                    "zero_speed_slope_ranges_pct": ranges_for_predicate(
                        slopes,
                        lambda slope, fs=flat_speed: current_speed_capped(fs, slope)
                        <= 1e-9,
                    ),
                    "capped_speed_slope_ranges_pct": ranges_for_predicate(
                        slopes,
                        lambda slope, fs=flat_speed: abs(
                            current_speed_capped(fs, slope) - fs * MAX_RATIO
                        )
                        <= 1e-9,
                    ),
                    "raw_negative_slope_ranges_pct": ranges_for_predicate(
                        slopes,
                        lambda slope, fs=flat_speed: bicycle_speed_raw(fs, slope)
                        < 0.0,
                    ),
                }
            )


def format_num(value: float) -> str:
    return f"{value:.6g}"


def svg_escape(value: str) -> str:
    return (
        value.replace("&", "&amp;")
        .replace("<", "&lt;")
        .replace(">", "&gt;")
        .replace('"', "&quot;")
    )


def write_svg_plot(
    path: Path,
    title: str,
    x_label: str,
    y_label: str,
    series: list[tuple[str, list[tuple[float, float]], str, float, str]],
    *,
    vertical_markers: list[tuple[float, str, str]] | None = None,
    horizontal_lines: list[tuple[float, str, str]] | None = None,
    width: int = 980,
    height: int = 620,
) -> None:
    margin_left = 72
    margin_right = 28
    margin_top = 56
    margin_bottom = 70
    plot_w = width - margin_left - margin_right
    plot_h = height - margin_top - margin_bottom

    all_points = [point for _, points, _, _, _ in series for point in points]
    x_min = min(x for x, _ in all_points)
    x_max = max(x for x, _ in all_points)
    y_min = min(0.0, min(y for _, y in all_points))
    y_max = max(y for _, y in all_points)
    y_pad = (y_max - y_min) * 0.08 or 1.0
    y_max += y_pad

    def x_px(value: float) -> float:
        return margin_left + (value - x_min) / (x_max - x_min) * plot_w

    def y_px(value: float) -> float:
        return margin_top + (y_max - value) / (y_max - y_min) * plot_h

    x_ticks = nice_ticks(x_min, x_max, 10)
    y_ticks = nice_ticks(y_min, y_max, 8)

    lines: list[str] = [
        f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="0 0 {width} {height}">',
        "<style>",
        "text { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', sans-serif; fill: #24313d; }",
        ".grid { stroke: #d8dee6; stroke-width: 1; }",
        ".axis { stroke: #344250; stroke-width: 1.4; }",
        ".tick { font-size: 12px; fill: #4c5b68; }",
        ".title { font-size: 20px; font-weight: 650; }",
        ".label { font-size: 14px; font-weight: 520; }",
        ".legend { font-size: 13px; }",
        ".marker { font-size: 11px; fill: #344250; }",
        "</style>",
        '<rect width="100%" height="100%" fill="#ffffff"/>',
        f'<text x="{margin_left}" y="32" class="title">{svg_escape(title)}</text>',
    ]

    for tick in x_ticks:
        x = x_px(tick)
        lines.append(
            f'<line x1="{x:.2f}" y1="{margin_top}" x2="{x:.2f}" y2="{margin_top + plot_h}" class="grid"/>'
        )
        lines.append(
            f'<text x="{x:.2f}" y="{margin_top + plot_h + 24}" text-anchor="middle" class="tick">{format_num(tick)}</text>'
        )

    for tick in y_ticks:
        y = y_px(tick)
        lines.append(
            f'<line x1="{margin_left}" y1="{y:.2f}" x2="{margin_left + plot_w}" y2="{y:.2f}" class="grid"/>'
        )
        lines.append(
            f'<text x="{margin_left - 10}" y="{y + 4:.2f}" text-anchor="end" class="tick">{format_num(tick)}</text>'
        )

    if horizontal_lines:
        for value, label, color in horizontal_lines:
            if y_min <= value <= y_max:
                y = y_px(value)
                lines.append(
                    f'<line x1="{margin_left}" y1="{y:.2f}" x2="{margin_left + plot_w}" y2="{y:.2f}" stroke="{color}" stroke-width="1.1" stroke-dasharray="6 3"/>'
                )
                lines.append(
                    f'<text x="{margin_left + plot_w - 6}" y="{y - 6:.2f}" text-anchor="end" class="marker">{svg_escape(label)}</text>'
                )

    if vertical_markers:
        for value, label, color in vertical_markers:
            if x_min <= value <= x_max:
                x = x_px(value)
                lines.append(
                    f'<line x1="{x:.2f}" y1="{margin_top}" x2="{x:.2f}" y2="{margin_top + plot_h}" stroke="{color}" stroke-width="1.1" stroke-dasharray="2 3"/>'
                )
                label_y = margin_top + 100
                lines.append(
                    f'<text x="{x - 4:.2f}" y="{label_y:.2f}" transform="rotate(-90 {x - 4:.2f} {label_y:.2f})" text-anchor="middle" class="marker">{svg_escape(label)}</text>'
                )

    if x_min <= 0.0 <= x_max:
        x0 = x_px(0.0)
        lines.append(
            f'<line x1="{x0:.2f}" y1="{margin_top}" x2="{x0:.2f}" y2="{margin_top + plot_h}" stroke="#78838f" stroke-width="1.5"/>'
        )
    if y_min <= 0.0 <= y_max:
        y0 = y_px(0.0)
        lines.append(
            f'<line x1="{margin_left}" y1="{y0:.2f}" x2="{margin_left + plot_w}" y2="{y0:.2f}" stroke="#78838f" stroke-width="1.5"/>'
        )

    lines.append(
        f'<rect x="{margin_left}" y="{margin_top}" width="{plot_w}" height="{plot_h}" fill="none" class="axis"/>'
    )

    for label, points, color, stroke_width, dash in series:
        coord = " ".join(f"{x_px(x):.2f},{y_px(y):.2f}" for x, y in points)
        dash_attr = f' stroke-dasharray="{dash}"' if dash else ""
        lines.append(
            f'<polyline points="{coord}" fill="none" stroke="{color}" stroke-width="{stroke_width}"{dash_attr} stroke-linejoin="round" stroke-linecap="round"/>'
        )

    lines.append(
        f'<text x="{margin_left + plot_w / 2:.2f}" y="{height - 22}" text-anchor="middle" class="label">{svg_escape(x_label)}</text>'
    )
    lines.append(
        f'<text x="20" y="{margin_top + plot_h / 2:.2f}" transform="rotate(-90 20 {margin_top + plot_h / 2:.2f})" text-anchor="middle" class="label">{svg_escape(y_label)}</text>'
    )

    legend_x = margin_left + plot_w - 190
    legend_y = margin_top + 18
    lines.append(
        f'<rect x="{legend_x - 14}" y="{legend_y - 18}" width="202" height="{22 * len(series) + 12}" fill="#ffffff" stroke="#d8dee6"/>'
    )
    for i, (label, _, color, stroke_width, dash) in enumerate(series):
        y = legend_y + i * 22
        dash_attr = f' stroke-dasharray="{dash}"' if dash else ""
        lines.append(
            f'<line x1="{legend_x}" y1="{y}" x2="{legend_x + 30}" y2="{y}" stroke="{color}" stroke-width="{stroke_width}"{dash_attr} stroke-linecap="round"/>'
        )
        lines.append(
            f'<text x="{legend_x + 38}" y="{y + 4}" class="legend">{svg_escape(label)}</text>'
        )

    lines.append("</svg>")
    path.write_text("\n".join(lines) + "\n")


def nice_ticks(low: float, high: float, target_count: int) -> list[float]:
    span = high - low
    if span <= 0:
        return [low]
    raw_step = span / max(target_count - 1, 1)
    magnitude = 10 ** math.floor(math.log10(raw_step))
    candidates = [1.0, 2.0, 2.5, 5.0, 10.0]
    step = min(candidates, key=lambda x: abs(raw_step - x * magnitude)) * magnitude
    start = math.ceil(low / step) * step
    ticks = []
    value = start
    while value <= high + step * 0.5:
        ticks.append(round(value, 10))
        value += step
    return ticks


def slope_points_for_plot(slope_min: float, slope_max: float, count: int) -> list[float]:
    return linspace(slope_min, slope_max, count)


def decimate(points: list[tuple[float, float]], max_points: int) -> list[tuple[float, float]]:
    if len(points) <= max_points:
        return points
    step = max(1, math.ceil(len(points) / max_points))
    reduced = points[::step]
    if reduced[-1] != points[-1]:
        reduced.append(points[-1])
    return reduced


def write_hybrid_lookup_plots(
    out_dir: Path,
    flat_speed: float,
    lookup_points: int,
    slope_min: float,
    slope_max: float,
    plot_points: int,
    model: Callable[[float, float], float],
    params: RoutingCurveParams,
) -> None:
    slopes = slope_points_for_plot(slope_min, slope_max, plot_points)
    table = build_lookup(flat_speed, slope_min, slope_max, lookup_points, model)

    current_points = [(s, current_speed_capped(flat_speed, s)) for s in slopes]
    hybrid_points = [(s, model(flat_speed, s)) for s in slopes]
    lookup_points_data = [(s, lookup_speed(table, s)) for s in slopes]
    curve_series: list[tuple[str, list[tuple[float, float]], str, float, str]] = [
        ("current AccessMod function", decimate(current_points, 1600), "#6b7280", 2.0, "7 4"),
        ("hybrid physics/walk-bike curve", decimate(hybrid_points, 1600), "#ff7f0e", 3.0, ""),
        (f"{lookup_points} point lookup", decimate(lookup_points_data, 1600), "#147a73", 1.7, "4 3"),
    ]
    error_points = [
        (s, abs(lookup_speed(table, s) - model(flat_speed, s)))
        for s in slopes
    ]
    error_series: list[tuple[str, list[tuple[float, float]], str, float, str]] = [
        (f"{lookup_points} point lookup error", decimate(error_points, 1600), "#147a73", 1.8, "")
    ]

    speed_label = format_num(flat_speed).replace(".", "_")
    write_svg_plot(
        out_dir / f"hybrid_bicycle_lookup_flat_{speed_label}.svg",
        f"Hybrid bicycle speed and lookup table, flat speed {format_num(flat_speed)} km/h",
        "slope (%)",
        "speed (km/h)",
        curve_series,
        vertical_markers=[
            (params.downhill_blocked_slope, "blocked / near-zero", "#0072ce"),
            (params.downhill_walk_slope, "walk bike downhill", "#0072ce"),
            (params.downhill_physics_slope, "physics range starts", "#0072ce"),
            (params.uphill_physics_slope, "physics range ends", "#0072ce"),
            (params.uphill_walk_slope, "walk bike uphill", "#0072ce"),
        ],
    )
    write_svg_plot(
        out_dir / f"hybrid_lookup_error_flat_{speed_label}.svg",
        f"Hybrid lookup interpolation error, flat speed {format_num(flat_speed)} km/h",
        "slope (%)",
        "absolute error (km/h)",
        error_series,
    )


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description=__doc__)
    base_dir = Path(__file__).resolve().parent
    parser.add_argument("--out-dir", type=Path, default=base_dir / "output")
    parser.add_argument("--slope-min", type=float, default=-40.0)
    parser.add_argument("--slope-max", type=float, default=40.0)
    parser.add_argument("--flat-speed", type=float, default=12.0)
    parser.add_argument(
        "--flat-speeds",
        type=float,
        nargs="+",
        default=[10.0, 12.0, 15.0, 20.0, 25.0, 30.0],
        help="Flat speeds used for the CSV error statistics.",
    )
    parser.add_argument(
        "--samples",
        type=int,
        nargs="+",
        default=[100],
        help="Lookup-table point counts used for error statistics.",
    )
    parser.add_argument(
        "--eval-points",
        type=int,
        default=10001,
        help="Dense point count for error statistics.",
    )
    parser.add_argument(
        "--plot-points",
        type=int,
        default=4001,
        help="Dense point count for generated plot data.",
    )
    parser.add_argument(
        "--downhill-blocked-slope",
        type=float,
        default=-25.0,
        help="Downhill slope where bicycle routing becomes near-zero.",
    )
    parser.add_argument(
        "--downhill-walk-slope",
        type=float,
        default=-18.0,
        help="Downhill slope where the cyclist is assumed to dismount.",
    )
    parser.add_argument(
        "--downhill-physics-slope",
        type=float,
        default=-5.0,
        help="Downhill slope where the physics riding range starts.",
    )
    parser.add_argument(
        "--uphill-physics-slope",
        type=float,
        default=20.0,
        help="Uphill slope where the physics riding range ends.",
    )
    parser.add_argument(
        "--uphill-walk-slope",
        type=float,
        default=25.0,
        help="Uphill slope where the cyclist is assumed to dismount.",
    )
    parser.add_argument(
        "--uphill-blocked-slope",
        type=float,
        default=35.0,
        help="Uphill slope where bicycle routing becomes near-zero.",
    )
    parser.add_argument(
        "--blocked-speed",
        type=float,
        default=0.25,
        help="Near-zero routing speed used beyond blocked thresholds, in km/h.",
    )
    parser.add_argument(
        "--walk-bike-flat-speed",
        type=float,
        default=3.5,
        help="Flat-terrain speed used for Tobler walk-bike fallback, in km/h.",
    )
    return parser.parse_args()


def main() -> None:
    args = parse_args()
    if args.slope_min >= args.slope_max:
        raise SystemExit("--slope-min must be lower than --slope-max")
    if any(count < 2 for count in args.samples):
        raise SystemExit("--samples values must all be >= 2")
    if args.eval_points < 2 or args.plot_points < 2:
        raise SystemExit("--eval-points and --plot-points must be >= 2")
    if not (
        args.slope_min
        <= args.downhill_blocked_slope
        < args.downhill_walk_slope
        < args.downhill_physics_slope
        < 0
        < args.uphill_physics_slope
        < args.uphill_walk_slope
        < args.uphill_blocked_slope
        <= args.slope_max
    ):
        raise SystemExit("slope boundary parameters must be ordered within the slope range")

    out_dir = args.out_dir
    out_dir.mkdir(parents=True, exist_ok=True)

    curve_params = RoutingCurveParams(
        downhill_blocked_slope=args.downhill_blocked_slope,
        downhill_walk_slope=args.downhill_walk_slope,
        downhill_physics_slope=args.downhill_physics_slope,
        uphill_physics_slope=args.uphill_physics_slope,
        uphill_walk_slope=args.uphill_walk_slope,
        uphill_blocked_slope=args.uphill_blocked_slope,
        blocked_speed=args.blocked_speed,
        walk_bike_flat_speed=args.walk_bike_flat_speed,
    )

    hybrid_model = lambda flat_speed, slope: hybrid_routing_speed(
        flat_speed, slope, curve_params
    )

    write_stats_csv(
        out_dir / "hybrid_lookup_error_stats.csv",
        args.flat_speeds,
        args.samples,
        args.slope_min,
        args.slope_max,
        args.eval_points,
        hybrid_model,
    )

    slopes = slope_points_for_plot(args.slope_min, args.slope_max, args.plot_points)
    diagnostic_slopes = slope_points_for_plot(
        args.slope_min, args.slope_max, args.eval_points
    )
    write_current_model_diagnostics_csv(
        out_dir / "current_model_diagnostics.csv",
        args.flat_speeds,
        diagnostic_slopes,
    )

    speed_label = format_num(args.flat_speed).replace(".", "_")
    write_model_csv(
        out_dir / f"model_curves_flat_{speed_label}.csv",
        args.flat_speed,
        slopes,
        curve_params,
    )
    write_hybrid_lookup_plots(
        out_dir,
        args.flat_speed,
        args.samples[0],
        args.slope_min,
        args.slope_max,
        args.plot_points,
        hybrid_model,
        curve_params,
    )

    print(f"Wrote lookup analysis outputs to {out_dir}")


if __name__ == "__main__":
    main()
