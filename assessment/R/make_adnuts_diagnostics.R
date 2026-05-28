#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(purrr)
  library(readr)
  library(stringr)
  library(tidyr)
})

run_specs <- tibble::tribble(
  ~model, ~hypothesis, ~run_label,
  "h1_0.12", "Single-stock", "scw16_adnuts_mle_4x1000_hbf",
  "h2_0.12", "Two-stock", "scw16_adnuts_mle_4x1000_hbf"
)

repo_root <- normalizePath(file.path(getwd(), ".."), winslash = "/")
doc_data_dir <- file.path(repo_root, "doc", "data")
doc_fig_dir <- file.path(repo_root, "doc", "figures", "adnuts")
dir.create(doc_data_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(doc_fig_dir, recursive = TRUE, showWarnings = FALSE)

safe_model_name <- function(model) {
  str_replace_all(model, "\\.", "_")
}

run_dir <- function(model, run_label) {
  file.path("mcmc_runs", model, run_label)
}

posterior_draws <- function(fit, parameters) {
  samples <- fit$samples
  start <- fit$warmup + 1L
  keep_iter <- seq.int(start, dim(samples)[[1]])
  chain_id <- seq_len(dim(samples)[[2]])
  draw_id <- seq_along(keep_iter)

  map_dfr(parameters, function(parameter) {
    values <- samples[keep_iter, chain_id, parameter, drop = FALSE]
    tibble(
      iteration = rep(draw_id, times = length(chain_id)),
      chain = factor(rep(chain_id, each = length(draw_id))),
      parameter = parameter,
      value = as.vector(values[, , 1])
    )
  })
}

wide_draws <- function(fit, parameters, max_draws = 2000L) {
  samples <- fit$samples
  start <- fit$warmup + 1L
  keep_iter <- seq.int(start, dim(samples)[[1]])
  chain_id <- seq_len(dim(samples)[[2]])

  draws <- map_dfc(parameters, function(parameter) {
    values <- samples[keep_iter, chain_id, parameter, drop = FALSE]
    tibble(!!parameter := as.vector(values[, , 1]))
  })

  draws <- draws %>%
    mutate(chain = factor(rep(chain_id, each = length(keep_iter)))) %>%
    relocate(chain)

  if (nrow(draws) > max_draws) {
    set.seed(20260528)
    draws <- draws[sample.int(nrow(draws), max_draws), , drop = FALSE]
  }

  draws
}

select_parameters <- function(monitor, n = 6L) {
  monitor %>%
    filter(parameter != "lp__", is.finite(Rhat), is.finite(n_eff)) %>%
    arrange(desc(Rhat), n_eff) %>%
    slice_head(n = n) %>%
    pull(parameter)
}

sampler_summary <- function(fit) {
  map_dfr(seq_along(fit$sampler_params), function(chain_id) {
    x <- fit$sampler_params[[chain_id]]
    keep <- seq.int(fit$warmup + 1L, nrow(x))
    tibble(
      chain = chain_id,
      divergences = sum(x[keep, "divergent__"], na.rm = TRUE),
      max_treedepth_hits = sum(x[keep, "treedepth__"] >= fit$max_treedepth, na.rm = TRUE)
    )
  })
}

plot_pairs <- function(draws, model, path) {
  params <- setdiff(names(draws), "chain")
  colors <- c("#1b9e77", "#d95f02", "#7570b3", "#e7298a")
  point_colors <- colors[as.integer(draws$chain)]

  png(path, width = 9, height = 9, units = "in", res = 180)
  on.exit(dev.off(), add = TRUE)
  pairs(
    draws[, params, drop = FALSE],
    pch = 16,
    cex = 0.22,
    col = grDevices::adjustcolor(point_colors, alpha.f = 0.35),
    gap = 0.25,
    cex.labels = 0.8,
    main = paste(model, "ADNUTS posterior pairs")
  )
}

plot_trace <- function(draws, model, path) {
  p <- ggplot(draws, aes(x = iteration, y = value, color = chain, group = chain)) +
    geom_line(linewidth = 0.2, alpha = 0.7) +
    facet_wrap(~parameter, scales = "free_y", ncol = 2) +
    scale_color_brewer(palette = "Dark2") +
    labs(
      title = paste(model, "ADNUTS trace plots"),
      x = "Post-warmup draw",
      y = "Parameter value",
      color = "Chain"
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")

  ggsave(path, p, width = 9, height = 7, dpi = 180)
}

plot_rhat_ess <- function(monitor, model, path) {
  plot_data <- monitor %>%
    filter(parameter != "lp__", is.finite(Rhat), is.finite(Bulk_ESS), Bulk_ESS > 0) %>%
    mutate(flag = Rhat > 1.01 | Bulk_ESS < 100)

  p <- ggplot(plot_data, aes(x = Bulk_ESS, y = Rhat, color = flag)) +
    geom_point(alpha = 0.55, size = 1.3) +
    geom_hline(yintercept = 1.01, linetype = "dashed", color = "#666666") +
    geom_vline(xintercept = 100, linetype = "dashed", color = "#666666") +
    scale_x_log10() +
    scale_color_manual(values = c("FALSE" = "#2c7fb8", "TRUE" = "#d95f0e"), guide = "none") +
    labs(
      title = paste(model, "ADNUTS Rhat and bulk ESS"),
      x = "Bulk effective sample size, log scale",
      y = "Rhat"
    ) +
    theme_bw(base_size = 11)

  ggsave(path, p, width = 8, height = 5, dpi = 180)
}

plot_sampler <- function(fit, model, path) {
  sampler <- map_dfr(seq_along(fit$sampler_params), function(chain_id) {
    as_tibble(fit$sampler_params[[chain_id]]) %>%
      mutate(iteration = row_number(), chain = factor(chain_id))
  }) %>%
    filter(iteration > fit$warmup) %>%
    select(iteration, chain, accept_stat__, treedepth__, divergent__, energy__) %>%
    pivot_longer(
      cols = c(accept_stat__, treedepth__, energy__),
      names_to = "metric",
      values_to = "value"
    ) %>%
    mutate(
      metric = recode(
        metric,
        accept_stat__ = "Acceptance statistic",
        treedepth__ = "Tree depth",
        energy__ = "Energy"
      )
    )

  p <- ggplot(sampler, aes(x = iteration, y = value, color = chain, group = chain)) +
    geom_line(linewidth = 0.25, alpha = 0.65) +
    facet_wrap(~metric, scales = "free_y", ncol = 1) +
    scale_color_brewer(palette = "Dark2") +
    labs(
      title = paste(model, "ADNUTS sampler diagnostics"),
      x = "Post-warmup draw",
      y = NULL,
      color = "Chain"
    ) +
    theme_bw(base_size = 11) +
    theme(legend.position = "bottom")

  ggsave(path, p, width = 8, height = 7, dpi = 180)
}

make_outputs <- function(model, hypothesis, run_label) {
  path <- run_dir(model, run_label)
  fit_path <- file.path(path, "fit.rds")
  monitor_path <- file.path(path, "monitor.csv")
  summary_path <- file.path(path, "summary.csv")

  if (!file.exists(fit_path)) {
    stop("Missing fit file: ", fit_path)
  }

  fit <- readRDS(fit_path)
  monitor <- read_csv(monitor_path, show_col_types = FALSE)
  sampler <- sampler_summary(fit) %>%
    mutate(model = model, .before = chain)
  summary <- read_csv(summary_path, show_col_types = FALSE) %>%
    mutate(
      divergences = sum(sampler$divergences),
      max_treedepth_hits = sum(sampler$max_treedepth_hits)
    ) %>%
    mutate(hypothesis = hypothesis, run_label = run_label, .before = model)

  parameters <- select_parameters(monitor)
  long <- posterior_draws(fit, parameters)
  wide <- wide_draws(fit, parameters)
  stem <- safe_model_name(model)

  plot_pairs(wide, model, file.path(doc_fig_dir, paste0(stem, "_pairs.png")))
  plot_trace(long, model, file.path(doc_fig_dir, paste0(stem, "_trace.png")))
  plot_rhat_ess(monitor, model, file.path(doc_fig_dir, paste0(stem, "_rhat_ess.png")))
  plot_sampler(fit, model, file.path(doc_fig_dir, paste0(stem, "_sampler.png")))

  slow <- monitor %>%
    filter(parameter != "lp__", is.finite(n_eff), is.finite(Rhat)) %>%
    arrange(n_eff) %>%
    slice_head(n = 10) %>%
    transmute(model = model, parameter, n_eff, Rhat)

  list(summary = summary, sampler = sampler, slow = slow)
}

outputs <- pmap(run_specs, make_outputs)

bind_rows(map(outputs, "summary")) %>%
  write_csv(file.path(doc_data_dir, "adnuts_mcmc_summary.csv"))

bind_rows(map(outputs, "sampler")) %>%
  write_csv(file.path(doc_data_dir, "adnuts_mcmc_sampler_diagnostics.csv"))

bind_rows(map(outputs, "slow")) %>%
  write_csv(file.path(doc_data_dir, "adnuts_mcmc_slow_parameters.csv"))
