input <- "docs/day3/fishing_trip_summary_1994_2026.csv"
output_png <- "docs/day3/fishing_breakpoint_patterns.png"
output_csv <- "docs/day3/fishing_breakpoint_periods.csv"

dat <- read.csv(input)
stopifnot(all(c(
  "year",
  "catch_proportion_referenced_st_percent",
  "number_of_fishing_sets"
) %in% names(dat)))

min_segment_years <- 5
years <- dat$year
n <- nrow(dat)
candidate <- seq(min_segment_years, n - min_segment_years)

segment_rss <- function(y, start, end) {
  idx <- start:end
  fit <- lm(y[idx] ~ years[idx])
  sum(resid(fit)^2)
}

shared_rss <- function(b1, b2, y1, y2) {
  segment_rss(y1, 1, b1) +
    segment_rss(y1, b1 + 1, b2) +
    segment_rss(y1, b2 + 1, n) +
    segment_rss(y2, 1, b1) +
    segment_rss(y2, b1 + 1, b2) +
    segment_rss(y2, b2 + 1, n)
}

y_catch <- as.numeric(scale(dat$catch_proportion_referenced_st_percent))
y_sets <- as.numeric(scale(log10(dat$number_of_fishing_sets)))

best <- data.frame(b1 = integer(), b2 = integer(), rss = numeric())
for (b1 in candidate) {
  for (b2 in candidate) {
    if ((b2 - b1) >= min_segment_years && (n - b2) >= min_segment_years) {
      best <- rbind(best, data.frame(b1 = b1, b2 = b2, rss = shared_rss(b1, b2, y_catch, y_sets)))
    }
  }
}

best <- best[which.min(best$rss), ]
break_years <- years[c(best$b1, best$b2)]

dat$period <- cut(
  dat$year,
  breaks = c(-Inf, break_years[1], break_years[2], Inf),
  labels = c(
    paste0(years[1], "-", break_years[1]),
    paste0(break_years[1] + 1, "-", break_years[2]),
    paste0(break_years[2] + 1, "-", years[n])
  ),
  right = TRUE
)

dat$catch_per_set_per_hold_set <- with(
  dat,
  catch_per_set_tons / (vessel_hold_capacity_m3 * number_of_fishing_sets)
)

period_summary <- aggregate(
  cbind(catch_proportion_referenced_st_percent, number_of_fishing_sets) ~ period,
  data = dat,
  FUN = mean
)
period_summary$start_year <- tapply(dat$year, dat$period, min)
period_summary$end_year <- tapply(dat$year, dat$period, max)
period_summary <- period_summary[, c(
  "period",
  "start_year",
  "end_year",
  "catch_proportion_referenced_st_percent",
  "number_of_fishing_sets"
)]
names(period_summary)[4:5] <- c("mean_catch_proportion_referenced_st_percent", "mean_number_of_fishing_sets")
write.csv(period_summary, output_csv, row.names = FALSE)

fit_segments <- function(y) {
  out <- numeric(n)
  for (idx in list(1:best$b1, (best$b1 + 1):best$b2, (best$b2 + 1):n)) {
    fit <- lm(y[idx] ~ years[idx])
    out[idx] <- predict(fit, data.frame("years[idx]" = years[idx]))
  }
  out
}

dat$catch_fit <- fit_segments(dat$catch_proportion_referenced_st_percent)
dat$sets_fit <- fit_segments(dat$number_of_fishing_sets)
dat$normalized_catch_fit <- fit_segments(dat$catch_per_set_per_hold_set)

png(output_png, width = 1600, height = 1320, res = 160)
op <- par(mfrow = c(3, 1), mar = c(4, 6, 3, 2), oma = c(0, 0, 3, 0), las = 1)
period_cols <- c("#EAF2FA", "#EEF8F1", "#FFF4E5")
plot_panel <- function(y, fit, ylab, main) {
  ylim <- range(c(y, fit), na.rm = TRUE)
  plot(years, y, type = "n", xlab = "", ylab = ylab, main = main, ylim = ylim)
  usr <- par("usr")
  rect(years[1] - 0.5, usr[3], break_years[1] + 0.5, usr[4], col = period_cols[1], border = NA)
  rect(break_years[1] + 0.5, usr[3], break_years[2] + 0.5, usr[4], col = period_cols[2], border = NA)
  rect(break_years[2] + 0.5, usr[3], years[n] + 0.5, usr[4], col = period_cols[3], border = NA)
  grid(col = "#D6DEE8")
  points(years, y, pch = 19, col = "#0B2545")
  lines(years, fit, lwd = 3, col = "#D78C34")
  abline(v = break_years + 0.5, lty = 2, lwd = 2, col = "#B64A3B")
  legend(
    "topleft",
    legend = c(levels(dat$period), "piecewise fit", "breakpoint"),
    fill = c(period_cols, NA, NA),
    border = c(rep(NA, 3), NA, NA),
    lty = c(NA, NA, NA, 1, 2),
    lwd = c(NA, NA, NA, 3, 2),
    col = c(rep(NA, 3), "#D78C34", "#B64A3B"),
    bty = "n",
    cex = 0.85
  )
}

plot_panel(
  dat$catch_proportion_referenced_st_percent,
  dat$catch_fit,
  "Catch proportion referenced ST (%)",
  "Catch proportion pattern"
)
plot_panel(
  dat$number_of_fishing_sets,
  dat$sets_fit,
  "Number of fishing sets",
  "Fishing set pattern"
)
plot_panel(
  dat$catch_per_set_per_hold_set,
  dat$normalized_catch_fit,
  "Catch/set per hold-set effort",
  "Normalized catch-rate pattern"
)
mtext(
  paste0("Shared two-breakpoint fit: ", break_years[1], " / ", break_years[2]),
  outer = TRUE,
  cex = 1.2,
  font = 2
)
par(op)
dev.off()

cat("Break years:", paste(break_years, collapse = ", "), "\n")
cat("Wrote:", output_png, "\n")
cat("Wrote:", output_csv, "\n")
