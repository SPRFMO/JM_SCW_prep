input <- "docs/day3/fishing_trip_summary_1994_2026.csv"
output_png <- "docs/day3/normalized_catch_rate_loess.png"
output_csv <- "docs/day3/normalized_catch_rate_values.csv"

dat <- read.csv(input)
dat$catch_per_set_per_hold_set <- with(
  dat,
  catch_per_set_tons / (vessel_hold_capacity_m3 * number_of_fishing_sets)
)
excluded_years <- c(1994, 2006, 2026)
dat <- dat[!dat$year %in% excluded_years, ]
write.csv(
  dat[, c(
    "year",
    "catch_per_set_tons",
    "vessel_hold_capacity_m3",
    "number_of_fishing_sets",
    "catch_per_set_per_hold_set"
  )],
  output_csv,
  row.names = FALSE
)

fit <- loess(catch_per_set_per_hold_set ~ year, data = dat, span = 0.55, degree = 1)
pred_year <- seq(min(dat$year), max(dat$year), length.out = 300)
pred <- predict(fit, newdata = data.frame(year = pred_year), se = TRUE)
upper <- pred$fit + 1.96 * pred$se.fit
lower <- pred$fit - 1.96 * pred$se.fit

png(output_png, width = 1400, height = 900, res = 160)
op <- par(mar = c(5, 7, 4, 2), las = 1)
ylim <- range(c(dat$catch_per_set_per_hold_set, lower, upper), na.rm = TRUE)
plot(
  dat$year,
  dat$catch_per_set_per_hold_set,
  type = "n",
  xlab = "Year",
  ylab = "",
  main = "Normalized catch-rate pattern",
  ylim = ylim
)
mtext("Catch/set per hold-set effort", side = 2, line = 4.7, las = 0)
grid(col = "#D6DEE8")
polygon(
  c(pred_year, rev(pred_year)),
  c(upper, rev(lower)),
  col = "#D78C3440",
  border = NA
)
lines(pred_year, pred$fit, lwd = 3, col = "#D78C34")
points(dat$year, dat$catch_per_set_per_hold_set, pch = 19, col = "#0B2545")
legend(
  "topright",
  legend = c("annual value", "LOESS smoother", "95% SE band"),
  pch = c(19, NA, 15),
  lty = c(NA, 1, NA),
  lwd = c(NA, 3, NA),
  col = c("#0B2545", "#D78C34", "#D78C3440"),
  pt.cex = c(1, NA, 2),
  bty = "n"
)
par(op)
dev.off()

cat("Wrote:", output_png, "\n")
cat("Wrote:", output_csv, "\n")
