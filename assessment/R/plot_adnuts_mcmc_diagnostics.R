#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(adnuts)
  library(ggplot2)
})

models <- c("h1_0.05", "h2_0.05")
run_label <- "report_diag_20260522"
run_root <- "mcmc_runs"
image_dir <- file.path("..", "doc", "images", "mcmc_diagnostics")
data_dir <- file.path("..", "doc", "data")

dir.create(image_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)

read_required_csv <- function(path) {
  if (!file.exists(path)) {
    stop("Required file not found: ", path, call. = FALSE)
  }
  read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
}

slow_parameters <- function(fit, n = 10) {
  monitor <- data.frame(
    parameter = rownames(fit$monitor),
    fit$monitor,
    row.names = NULL,
    check.names = FALSE
  )
  monitor <- subset(monitor, parameter != "lp__")
  head(monitor[order(monitor$n_eff, monitor$Rhat), c("parameter", "n_eff", "Rhat")], n)
}

png_base <- function(path, width = 2200, height = 2200, res = 180, expr) {
  png(path, width = width, height = height, res = res)
  on.exit(dev.off(), add = TRUE)
  force(expr)
}

plot_model <- function(model) {
  run_dir <- file.path(run_root, model, run_label)
  fit_file <- file.path(run_dir, "fit.rds")
  if (!file.exists(fit_file)) {
    stop("Missing fit object for ", model, ": ", fit_file, call. = FALSE)
  }

  fit <- readRDS(fit_file)
  summary <- read_required_csv(file.path(run_dir, "summary.csv"))
  sampler <- read_required_csv(file.path(run_dir, "sampler_diagnostics.csv"))
  sampler$model <- model
  sampler <- sampler[, c("model", setdiff(names(sampler), "model"))]
  slow <- slow_parameters(fit)
  slow$model <- model
  slow <- slow[, c("model", "parameter", "n_eff", "Rhat")]

  png_base(
    file.path(image_dir, paste0(model, "_pairs_slow.png")),
    expr = {
      pairs_admb(fit, order = "slow", pars = 1:10, diag = "trace", label.cex = 0.58)
      mtext(
        paste(model, "ADNuts pairs plot: 10 slowest mixing parameters"),
        outer = TRUE,
        side = 3,
        line = 0.4,
        cex = 1.1,
        font = 2
      )
    }
  )

  png_base(
    file.path(image_dir, paste0(model, "_marginals_slow.png")),
    width = 1800,
    height = 1400,
    expr = {
      plot_marginals(fit, pars = slow$parameter, mfrow = c(3, 4), breaks = 25)
      mtext(
        paste(model, "marginal posteriors for slowest mixing parameters"),
        outer = TRUE,
        side = 3,
        line = -0.2,
        cex = 1.1,
        font = 2
      )
    }
  )

  sampler_plot <- plot_sampler_params(fit, plot = FALSE) +
    ggtitle(paste(model, "ADNuts sampler diagnostics")) +
    theme(plot.title = element_text(face = "bold"))
  ggsave(
    filename = file.path(image_dir, paste0(model, "_sampler_params.png")),
    plot = sampler_plot,
    width = 9,
    height = 11,
    dpi = 180
  )

  png_base(
    file.path(image_dir, paste0(model, "_uncertainties.png")),
    width = 1600,
    height = 1400,
    expr = {
      plot_uncertainties(fit, log = TRUE)
      title(paste(model, "posterior SD vs MLE SE"))
    }
  )

  list(summary = summary, sampler = sampler, slow = slow)
}

out <- lapply(models, plot_model)

summary_out <- do.call(rbind, lapply(out, `[[`, "summary"))
sampler_out <- do.call(rbind, lapply(out, `[[`, "sampler"))
slow_out <- do.call(rbind, lapply(out, `[[`, "slow"))

write.csv(summary_out, file.path(data_dir, "adnuts_mcmc_summary.csv"), row.names = FALSE)
write.csv(sampler_out, file.path(data_dir, "adnuts_mcmc_sampler_diagnostics.csv"), row.names = FALSE)
write.csv(slow_out, file.path(data_dir, "adnuts_mcmc_slow_parameters.csv"), row.names = FALSE)

cat("Wrote ADNuts MCMC diagnostic plots to ", normalizePath(image_dir), "\n", sep = "")
