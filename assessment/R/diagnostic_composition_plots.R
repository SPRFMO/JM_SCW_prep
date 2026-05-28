diagnostic_empty_plot <- function(label) {
  ggplot2::ggplot() +
    ggplot2::annotate("text", x = 0, y = 0, label = label) +
    ggplot2::xlim(-1, 1) +
    ggplot2::ylim(-1, 1) +
    ggplot2::theme_void()
}

diagnostic_theme <- function(base_size = 12) {
  if (exists("theme_jjm", mode = "function")) {
    theme_jjm(base_size = base_size)
  } else {
    ggplot2::theme_bw(base_size = base_size)
  }
}

diagnostic_series_names <- function(x) {
  if (is.matrix(x)) {
    as.character(x[, 1])
  } else {
    as.character(x)
  }
}

diagnostic_comp_bins <- function(mod, n_bins, comp_type) {
  if (comp_type == "age") {
    age_range <- as.numeric(mod[[1]]$data$ages)
    bins <- if (length(age_range) >= 2) seq(age_range[1], age_range[2]) else seq_len(n_bins)
  } else {
    bins <- as.numeric(mod[[1]]$data$lengthbin)
  }

  if (length(bins) != n_bins) {
    bins <- seq_len(n_bins)
  }

  bins
}

diagnostic_sample_matrix <- function(dat, source_type, comp_type) {
  if (source_type == "fsh" && comp_type == "age") {
    return(dat$Fagesample)
  }
  if (source_type == "fsh" && comp_type == "length") {
    return(dat$Flengthsample)
  }
  if (source_type == "ind" && comp_type == "age") {
    return(dat$Iagesample)
  }
  if (source_type == "ind" && comp_type == "length") {
    return(dat$Ilengthsample)
  }
  NULL
}

diagnostic_year_matrix <- function(dat, source_type, comp_type) {
  if (source_type == "fsh" && comp_type == "age") {
    return(dat$Fageyears)
  }
  if (source_type == "fsh" && comp_type == "length") {
    return(dat$Flengthyears)
  }
  if (source_type == "ind" && comp_type == "age") {
    return(dat$Iyearsage)
  }
  if (source_type == "ind" && comp_type == "length") {
    return(dat$Iyearslength)
  }
  NULL
}

diagnostic_sample_sizes <- function(dat, source_type, comp_type, source_id, years) {
  samples <- diagnostic_sample_matrix(dat, source_type, comp_type)
  sample_years <- diagnostic_year_matrix(dat, source_type, comp_type)
  out <- rep(NA_real_, length(years))

  if (is.null(samples) || source_id > ncol(samples)) {
    return(out)
  }

  if (!is.null(rownames(samples))) {
    out <- as.numeric(samples[match(as.character(years), rownames(samples)), source_id])
  }

  if (anyNA(out) && !is.null(sample_years) && source_id <= ncol(sample_years)) {
    idx <- match(years, as.numeric(sample_years[, source_id]))
    replace <- is.na(out) & !is.na(idx)
    out[replace] <- as.numeric(samples[idx[replace], source_id])
  }

  out
}

diagnostic_comp_names <- function(dat, source_type) {
  if (source_type == "fsh") {
    diagnostic_series_names(dat$Fnames)
  } else {
    diagnostic_series_names(dat$Inames)
  }
}

diagnostic_comp_extract_one <- function(mod, stock, source_type, source_id,
                                        comp_type) {
  dat <- mod[[1]]$data
  out <- mod[[1]]$output[[stock]]
  prefix <- if (comp_type == "age") {
    if (source_type == "fsh") "fsh" else "ind"
  } else {
    if (source_type == "fsh") "len_fsh" else "len_ind"
  }
  obs_name <- paste0("pobs_", prefix, "_", source_id)
  fit_name <- paste0("phat_", prefix, "_", source_id)

  if (!all(c(obs_name, fit_name) %in% names(out))) {
    return(NULL)
  }

  obs <- as.matrix(out[[obs_name]])
  fit <- as.matrix(out[[fit_name]])
  if (!nrow(obs) || !nrow(fit)) {
    return(NULL)
  }

  n_bins <- min(ncol(obs), ncol(fit)) - 1
  if (n_bins < 2) {
    return(NULL)
  }

  years <- as.numeric(obs[, 1])
  obs_values <- obs[, seq_len(n_bins) + 1, drop = FALSE]
  fit_values <- fit[, seq_len(n_bins) + 1, drop = FALSE]
  bins <- diagnostic_comp_bins(mod, n_bins, comp_type)
  source_names <- diagnostic_comp_names(dat, source_type)
  source_name <- if (source_id <= length(source_names)) {
    source_names[source_id]
  } else {
    paste(source_type, source_id)
  }
  sample_size <- diagnostic_sample_sizes(
    dat, source_type, comp_type, source_id, years
  )

  tibble::tibble(
    stock = stock,
    source_type = source_type,
    source_id = source_id,
    source_name = source_name,
    component = comp_type,
    year = rep(years, each = n_bins),
    bin_index = rep(seq_len(n_bins), times = length(years)),
    bin = rep(bins, times = length(years)),
    observed = as.vector(t(obs_values)),
    fitted = as.vector(t(fit_values)),
    sample_size = rep(sample_size, each = n_bins)
  )
}

diagnostic_comp_data <- function(mod, stock = 1, comp_type = c("age", "length")) {
  comp_type <- match.arg(comp_type)
  out <- mod[[1]]$output[[stock]]
  if (is.null(out)) {
    return(tibble::tibble())
  }

  patterns <- if (comp_type == "age") {
    c(fsh = "^pobs_fsh_([0-9]+)$", ind = "^pobs_ind_([0-9]+)$")
  } else {
    c(fsh = "^pobs_len_fsh_([0-9]+)$", ind = "^pobs_len_ind_([0-9]+)$")
  }

  pieces <- list()
  for (source_type in names(patterns)) {
    nms <- grep(patterns[[source_type]], names(out), value = TRUE)
    ids <- as.integer(sub(patterns[[source_type]], "\\1", nms))
    for (source_id in ids) {
      pieces[[length(pieces) + 1]] <- diagnostic_comp_extract_one(
        mod, stock, source_type, source_id, comp_type
      )
    }
  }

  dplyr::bind_rows(pieces)
}

diagnostic_multinomial_counts <- function(obs, n) {
  if (!is.finite(n) || n <= 0 || !any(is.finite(obs)) || sum(obs, na.rm = TRUE) <= 0) {
    return(NULL)
  }

  n <- max(1L, as.integer(round(n)))
  obs <- pmax(replace(obs, !is.finite(obs), 0), 0)
  obs <- obs / sum(obs)
  raw <- obs * n
  counts <- floor(raw)
  remainder <- n - sum(counts)

  if (remainder > 0) {
    add_to <- order(raw - counts, decreasing = TRUE)[seq_len(remainder)]
    counts[add_to] <- counts[add_to] + 1L
  }

  counts
}

diagnostic_osa_row <- function(obs, fitted, n) {
  counts <- diagnostic_multinomial_counts(obs, n)
  if (is.null(counts) || length(counts) < 2) {
    return(NULL)
  }

  fitted <- pmax(replace(fitted, !is.finite(fitted), 0), 1e-12)
  if (sum(fitted) <= 0) {
    return(NULL)
  }
  fitted <- fitted / sum(fitted)

  residual <- rep(NA_real_, length(counts) - 1L)
  for (i in seq_along(residual)) {
    n_remaining <- sum(counts[i:length(counts)])
    p_remaining <- sum(fitted[i:length(fitted)])
    if (n_remaining <= 0 || p_remaining <= 0) {
      next
    }

    p_i <- min(max(fitted[i] / p_remaining, 1e-12), 1 - 1e-12)
    lower <- stats::pbinom(counts[i] - 1L, n_remaining, p_i)
    upper <- stats::pbinom(counts[i], n_remaining, p_i)
    u <- min(max((lower + upper) / 2, 1e-7), 1 - 1e-7)
    residual[i] <- stats::qnorm(u)
  }

  residual[is.finite(residual)]
}

diagnostic_osa_data <- function(comp_df) {
  if (!nrow(comp_df)) {
    return(tibble::tibble())
  }

  comp_df <- comp_df |>
    dplyr::filter(is.finite(observed), is.finite(fitted)) |>
    dplyr::arrange(source_type, source_id, year, bin_index)

  groups <- split(
    comp_df,
    list(comp_df$stock, comp_df$source_type, comp_df$source_id, comp_df$year),
    drop = TRUE
  )

  pieces <- lapply(groups, function(x) {
    residual <- diagnostic_osa_row(x$observed, x$fitted, x$sample_size[1])
    if (is.null(residual) || !length(residual)) {
      return(NULL)
    }

    tibble::tibble(
      stock = x$stock[1],
      source_type = x$source_type[1],
      source_id = x$source_id[1],
      source_name = x$source_name[1],
      component = x$component[1],
      year = x$year[1],
      residual = residual
    )
  })

  dplyr::bind_rows(pieces)
}

plot_osa_qq_diagnostics <- function(comp_df, model_label = NULL) {
  osa_df <- diagnostic_osa_data(comp_df)
  if (!nrow(osa_df)) {
    return(diagnostic_empty_plot("No composition residuals available."))
  }

  qq_df <- osa_df |>
    dplyr::group_by(source_name) |>
    dplyr::arrange(residual, .by_group = TRUE) |>
    dplyr::mutate(theoretical = stats::qnorm(stats::ppoints(dplyr::n()))) |>
    dplyr::ungroup()

  sdnr_df <- osa_df |>
    dplyr::group_by(source_name) |>
    dplyr::summarise(
      sdnr = stats::sd(residual, na.rm = TRUE),
      .groups = "drop"
    )

  ggplot2::ggplot(qq_df, ggplot2::aes(theoretical, residual)) +
    ggplot2::geom_abline(intercept = 0, slope = 1, colour = "#d95f02", linewidth = 0.7) +
    ggplot2::geom_point(size = 0.9, alpha = 0.6) +
    ggplot2::geom_text(
      data = sdnr_df,
      ggplot2::aes(x = -Inf, y = Inf, label = sprintf("SDNR = %.2f", sdnr)),
      inherit.aes = FALSE,
      hjust = -0.05,
      vjust = 1.15,
      size = 3.2
    ) +
    ggplot2::facet_wrap(~source_name, scales = "free", ncol = 3) +
    ggplot2::labs(
      title = "OSA Residual QQ Diagnostics",
      subtitle = model_label,
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    ) +
    diagnostic_theme(base_size = 12) +
    ggplot2::theme(legend.position = "none")
}

plot_aggregate_composition_fits <- function(comp_df, model_label = NULL) {
  if (!nrow(comp_df)) {
    return(diagnostic_empty_plot("No composition fits available."))
  }

  comp_label <- paste0(
    toupper(substr(comp_df$component[1], 1, 1)),
    substr(comp_df$component[1], 2, nchar(comp_df$component[1]))
  )

  agg_df <- comp_df |>
    dplyr::filter(is.finite(observed), is.finite(fitted)) |>
    dplyr::mutate(
      sample_weight = dplyr::if_else(
        is.finite(sample_size) & sample_size > 0,
        sample_size,
        1
      )
    ) |>
    dplyr::group_by(source_name, component, bin_index, bin) |>
    dplyr::summarise(
      observed = sum(observed * sample_weight) / sum(sample_weight),
      fitted = sum(fitted * sample_weight) / sum(sample_weight),
      .groups = "drop"
    )

  if (!nrow(agg_df)) {
    return(diagnostic_empty_plot("No composition fits available."))
  }

  ggplot2::ggplot(agg_df, ggplot2::aes(bin)) +
    ggplot2::geom_col(
      ggplot2::aes(y = observed),
      fill = "#b9d9e8",
      colour = "#1f9bcf",
      width = 0.85,
      alpha = 0.9
    ) +
    ggplot2::geom_line(
      ggplot2::aes(y = fitted, group = source_name),
      colour = "#e41a1c",
      linewidth = 0.8
    ) +
    ggplot2::geom_point(ggplot2::aes(y = fitted), colour = "#e41a1c", size = 1.5) +
    ggplot2::facet_wrap(~source_name, scales = "free_y", ncol = 3) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(
      title = paste("Sample-Size Weighted Aggregate", comp_label, "Composition Fits"),
      subtitle = model_label,
      x = comp_label,
      y = "Proportion"
    ) +
    diagnostic_theme(base_size = 12) +
    ggplot2::theme(legend.position = "none")
}

plot_agecomp_fit <- function(data, source_name, ncol = 4) {
  pred_col <- if ("predicted" %in% names(data)) "predicted" else "fitted"
  if (!pred_col %in% names(data)) {
    return(diagnostic_empty_plot("No predicted age-composition fits available."))
  }

  plot_data <- data |>
    dplyr::filter(
      .data$source_name == .env$source_name,
      is.finite(.data$observed) | is.finite(.data[[pred_col]])
    ) |>
    dplyr::transmute(
      year = as.integer(.data$year),
      age = as.numeric(.data$bin),
      observed = as.numeric(.data$observed),
      predicted = as.numeric(.data[[pred_col]])
    ) |>
    dplyr::group_by(.data$year, .data$age) |>
    dplyr::summarise(
      observed = if (all(is.na(.data$observed))) NA_real_ else mean(.data$observed, na.rm = TRUE),
      predicted = if (all(is.na(.data$predicted))) NA_real_ else mean(.data$predicted, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$year, .data$age) |>
    dplyr::mutate(
      year = factor(.data$year, levels = sort(unique(.data$year)))
    )

  if (!nrow(plot_data)) {
    return(diagnostic_empty_plot(paste("No age-composition fits for", source_name)))
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$age)) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$predicted),
      linewidth = 0.5,
      color = "#4daf4a",
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$observed),
      color = "#1f78b4",
      size = 0.9,
      alpha = 0.8,
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(~year, ncol = ncol, dir = "v") +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(
      x = "Age",
      y = "Proportion",
      title = paste(source_name, "Age Composition Fits")
    ) +
    diagnostic_theme(base_size = 11) +
    ggplot2::theme(legend.position = "none")
}

plot_lengthcomp_fit <- function(data, source_name, ncol = 4) {
  pred_col <- if ("predicted" %in% names(data)) "predicted" else "fitted"
  if (!pred_col %in% names(data)) {
    return(diagnostic_empty_plot("No predicted length-composition fits available."))
  }

  plot_data <- data |>
    dplyr::filter(
      .data$source_name == .env$source_name,
      is.finite(.data$observed) | is.finite(.data[[pred_col]])
    ) |>
    dplyr::mutate(
      length = .data$bin,
      predicted = .data[[pred_col]]
    )

  if (!nrow(plot_data)) {
    return(diagnostic_empty_plot(paste("No length-composition fits for", source_name)))
  }

  ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$length)) +
    ggplot2::geom_line(
      ggplot2::aes(y = .data$predicted),
      linewidth = 0.5,
      color = "#4daf4a",
      na.rm = TRUE
    ) +
    ggplot2::geom_point(
      ggplot2::aes(y = .data$observed),
      color = "#1f78b4",
      size = 0.9,
      alpha = 0.8,
      na.rm = TRUE
    ) +
    ggplot2::facet_wrap(~year, ncol = ncol, dir = "v") +
    ggplot2::scale_y_continuous(limits = c(0, NA)) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks()) +
    ggplot2::labs(
      x = "Length",
      y = "Proportion",
      title = paste(source_name, "Length Composition Fits")
    ) +
    diagnostic_theme(base_size = 11) +
    ggplot2::theme(legend.position = "none")
}

plot_composition_diagnostics <- function(mod, stock = 1,
                                         comp_type = c("age", "length"),
                                         model_label = NULL) {
  comp_type <- match.arg(comp_type)
  comp_df <- diagnostic_comp_data(mod, stock = stock, comp_type = comp_type)
  if (!nrow(comp_df)) {
    return(diagnostic_empty_plot("No composition data available."))
  }

  patchwork::wrap_plots(
    plot_osa_qq_diagnostics(comp_df, model_label = model_label),
    plot_aggregate_composition_fits(comp_df, model_label = model_label),
    ncol = 1,
    heights = c(1, 1.05)
  )
}

plot_diagnostics <- plot_composition_diagnostics
