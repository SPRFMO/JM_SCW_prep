library(patchwork)
library(scales)
library(ggridges)

# ---------------------------------------------------------------------------
# PFA brand colours (official palette)
# ---------------------------------------------------------------------------
PFA_BLUE    <- "#004868"   # dark teal (primary)
PFA_ORANGE  <- "#FF800A"   # bright orange (secondary)
PFA_GREEN   <- "#00B764"   # bright green (tertiary)
PFA_BLUE2   <- "#00BADC"   # bright cyan (quaternary)
PFA_ORANGE2 <- "#F2A44A"   # amber tint (derived; used for gradients)

# Distinct colours for retrospective peels (peel 0 = PFA_BLUE; these for peels 1-5)
.PEEL_PAL <- c(PFA_ORANGE, PFA_GREEN, PFA_ORANGE2, PFA_BLUE2, "#808080")

# Discrete palettes: up to six fleets / surveys
FLEET_PAL <- c(PFA_BLUE, PFA_BLUE2, PFA_ORANGE, PFA_GREEN, "#808080", "#666666", PFA_ORANGE2, "#6B4C9A")
SURV_PAL  <- c(PFA_ORANGE, PFA_BLUE, PFA_GREEN, PFA_BLUE2, "#808080", "#666666", PFA_ORANGE2, "#6B4C9A")

# ---------------------------------------------------------------------------
# Publication theme
# ---------------------------------------------------------------------------
theme_jjm <- function(base_size = 12) {
  theme_bw(base_size = base_size) +
    theme(
      panel.grid.minor  = element_blank(),
      panel.grid.major  = element_line(colour = "grey92", linewidth = 0.4),
      strip.background  = element_rect(fill = "grey95", colour = "grey70"),
      strip.text        = element_text(size = rel(0.85), face = "bold"),
      legend.position   = "bottom",
      legend.key        = element_blank(),
      legend.title      = element_text(size = rel(0.9)),
      axis.ticks        = element_line(linewidth = 0.4),
      plot.title        = element_text(size = rel(1), face = "bold"),
      plot.subtitle     = element_text(size = rel(0.85), colour = "grey40")
    )
}

# Build a consistent plot title from a model name string (e.g. "h1_1.14")
# Optional stock_name appended after an em-dash.
jjm_label <- function(nm, stock_name = NULL) {
  hyp <- if (startsWith(nm, "h1")) "Single-stock" else "Two-stock"
  ver <- sub("^h[12]_", "", nm)
  lbl <- paste0(hyp, " – ", ver)
  if (!is.null(stock_name)) lbl <- paste0(lbl, " – ", stock_name)
  lbl
}

# ---------------------------------------------------------------------------
# Data-extraction helpers
# All accept a jjm.output object (mod) and stock index (default 1)
# ---------------------------------------------------------------------------

.jjm_out <- function(mod, stock = 1) mod[[1]]$output[[stock]]

# Read actual length-bin values (cm) from a JJM dat file.
# #lengthbin always appears within the first ~15 lines, so only read 20.
read_lenbins <- function(dat_path) {
  lines <- readLines(dat_path, n = 20L, warn = FALSE)
  idx   <- which(trimws(lines) == "#lengthbin")
  if (!length(idx)) return(NULL)
  as.integer(strsplit(trimws(lines[idx[1L] + 1L]), "\\s+")[[1]])
}

# Derive the dat file path from the ctl file: reads line 2 of the ctl file.
dat_path_from_ctl <- function(model_name, config_path, input_path) {
  ctl  <- readLines(file.path(config_path, paste0(model_name, ".ctl")), n = 2L, warn = FALSE)
  file.path(input_path, trimws(ctl[2L]))
}
.fnames  <- function(out) {
  n <- out$Fshry_names
  if (is.matrix(n)) n[, 1] else as.character(n)
}
.snames  <- function(out) {
  n <- out$Index_names
  if (is.matrix(n)) n[, 1] else as.character(n)
}

# Catch by fleet – long format
extr_catch <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)
  fn  <- .fnames(out)
  idx <- grep("^Obs_catch_", names(out))
  purrr::imap_dfr(idx, \(ci, i)
    tibble(year = out$Yr, catch = out[[ci]], fleet = fn[i])
  ) |>
    mutate(fleet = factor(fleet, levels = fn))
}

# Weight-at-age – fisheries
extr_wtat_fsh <- function(mod, stock = 1) {
  out  <- .jjm_out(mod, stock)
  fn   <- .fnames(out)
  cols <- grep("^wt_fsh_", names(out), value = TRUE)
  purrr::imap_dfr(cols, \(cn, i) {
    mat   <- as.matrix(out[[cn]])   # [n_yr x (1 + n_age)]
    vals  <- mat[, -1, drop = FALSE]
    n_age <- ncol(vals)
    as.data.frame(vals) |>
      setNames(paste0("a", seq_len(n_age))) |>
      mutate(year = mat[, 1], fleet = fn[i]) |>
      pivot_longer(starts_with("a"), names_to = "age", values_to = "weight",
                   names_transform = list(age = \(x) as.integer(sub("a", "", x))))
  })
}

# Weight-at-age – surveys
extr_wtat_srv <- function(mod, stock = 1) {
  out  <- .jjm_out(mod, stock)
  sn   <- .snames(out)
  cols <- grep("^wt_ind_", names(out), value = TRUE)
  purrr::imap_dfr(cols, \(cn, i) {
    mat   <- as.matrix(out[[cn]])
    vals  <- mat[, -1, drop = FALSE]
    n_age <- ncol(vals)
    as.data.frame(vals) |>
      setNames(paste0("a", seq_len(n_age))) |>
      mutate(year = mat[, 1], survey = sn[i]) |>
      pivot_longer(starts_with("a"), names_to = "age", values_to = "weight",
                   names_transform = list(age = \(x) as.integer(sub("a", "", x))))
  })
}

# Age-composition fits – fisheries
extr_agefits_fsh <- function(mod, stock = 1) {
  out      <- .jjm_out(mod, stock)
  fn       <- .fnames(out)
  obs_cols <- grep("^pobs_fsh_", names(out), value = TRUE)
  hat_cols <- grep("^phat_fsh_", names(out), value = TRUE)

  .lng <- \(mat, tp) {
    mat   <- as.matrix(mat)
    vals  <- mat[, -1, drop = FALSE]
    n_age <- ncol(vals)
    as.data.frame(vals) |>
      setNames(paste0("a", seq_len(n_age))) |>
      mutate(year = mat[, 1], type = tp) |>
      pivot_longer(starts_with("a"), names_to = "age", values_to = "prop",
                   names_transform = list(age = \(x) as.integer(sub("a", "", x))))
  }
  obs_nums <- sort(as.integer(sub("^pobs_fsh_", "", obs_cols)))
  purrr::imap_dfr(obs_cols, \(cn, i) {
    fleet_num  <- as.integer(sub("^pobs_fsh_", "", cn))
    hat_cn     <- paste0("phat_fsh_", fleet_num)
    fleet_name <- if (length(fn) >= max(obs_nums)) fn[fleet_num] else fn[match(fleet_num, obs_nums)]
    bind_rows(.lng(out[[cn]], "observed"),
              .lng(out[[hat_cn]], "predicted")) |>
      mutate(fleet = fleet_name)
  })
}

# Length-composition fits – fisheries
# lenbins: real cm values from read_lenbins(); if NULL falls back to 1:n indices
extr_lenfits_fsh <- function(mod, stock = 1, lenbins = NULL) {
  out      <- .jjm_out(mod, stock)
  fn       <- .fnames(out)
  obs_cols <- grep("^pobs_len_fsh_", names(out), value = TRUE)
  hat_cols <- grep("^phat_len_fsh_", names(out), value = TRUE)
  if (!length(obs_cols)) return(tibble(year = integer(), length_bin = integer(),
                                      prop = numeric(), type = character(),
                                      fleet = character()))

  .lng <- \(mat, tp) {
    vals <- mat[, -1]
    n    <- ncol(vals)
    bins <- if (!is.null(lenbins) && length(lenbins) == n) lenbins else seq_len(n)
    as.data.frame(vals) |>
      setNames(paste0("l", seq_len(n))) |>
      mutate(year = mat[, 1], type = tp) |>
      pivot_longer(starts_with("l"), names_to = ".idx", values_to = "prop",
                   names_transform = list(.idx = \(x) as.integer(sub("l", "", x)))) |>
      mutate(length_bin = bins[.idx]) |>
      select(-.idx)
  }
  obs_nums <- sort(as.integer(sub("^pobs_len_fsh_", "", obs_cols)))
  purrr::imap_dfr(obs_cols, \(cn, i) {
    fleet_num  <- as.integer(sub("^pobs_len_fsh_", "", cn))
    hat_cn     <- paste0("phat_len_fsh_", fleet_num)
    fleet_name <- if (length(fn) >= max(obs_nums)) fn[fleet_num] else fn[match(fleet_num, obs_nums)]
    bind_rows(.lng(out[[cn]], "observed"),
              .lng(out[[hat_cn]], "predicted")) |>
      mutate(fleet = fleet_name)
  })
}

# Age-composition fits – surveys
extr_agefits_srv <- function(mod, stock = 1) {
  out      <- .jjm_out(mod, stock)
  sn       <- .snames(out)
  obs_cols <- grep("^pobs_ind_", names(out), value = TRUE)
  hat_cols <- grep("^phat_ind_", names(out), value = TRUE)

  .lng <- \(mat, tp) {
    mat   <- as.matrix(mat)
    vals  <- mat[, -1, drop = FALSE]
    n_age <- ncol(vals)
    as.data.frame(vals) |>
      setNames(paste0("a", seq_len(n_age))) |>
      mutate(year = mat[, 1], type = tp) |>
      pivot_longer(starts_with("a"), names_to = "age", values_to = "prop",
                   names_transform = list(age = \(x) as.integer(sub("a", "", x))))
  }
  purrr::imap_dfr(obs_cols, \(cn, i)
    bind_rows(.lng(out[[cn]], "observed"),
              .lng(out[[hat_cols[i]]], "predicted")) |>
      mutate(survey = sn[i])
  )
}

# Survey index fits (normalised within each survey)
extr_indices <- function(mod, stock = 1) {
  out      <- .jjm_out(mod, stock)
  sn       <- .snames(out)
  obs_cols <- grep("^Obs_Survey_", names(out), value = TRUE)
  purrr::imap_dfr(obs_cols, \(cn, i) {
    if (i > length(sn) || is.na(sn[i])) return(NULL)       # guard: unnamed survey
    df <- as.data.frame(out[[cn]])
    colnames(df)[1:4] <- c("year", "obs", "model", "sd")
    if (!any(df$obs > 0, na.rm = TRUE)) return(NULL)        # no real observations in raw data
    sc <- max(c(df$obs, df$model, df$sd), na.rm = TRUE)
    if (!is.finite(sc) || sc <= 0) return(NULL)
    df |>
      mutate(obs    = obs   / sc,
             model  = model / sc,
             sd     = sd    / sc,
             obs    = if_else(obs <= 0, NA_real_, obs),
             survey = sn[i]) |>
      filter(!is.na(model), model > 0) |>
      select(year, obs, model, sd, survey)
  })
}

# Mean age – fisheries
extr_meanage_fsh <- function(mod, stock = 1) {
  out      <- .jjm_out(mod, stock)
  fn       <- .fnames(out)
  eff_cols <- grep("^EffN_Fsh_", names(out), value = TRUE)
  eff_nums <- sort(as.integer(sub("^EffN_Fsh_", "", eff_cols)))
  purrr::imap_dfr(eff_cols, \(cn, i) {
    fleet_num  <- as.integer(sub("^EffN_Fsh_", "", cn))
    fleet_name <- if (length(fn) >= max(eff_nums)) fn[fleet_num] else fn[match(fleet_num, eff_nums)]
    df <- as.data.frame(out[[cn]])
    if (ncol(df) < 8) return(NULL)
    df[, c(1, 4, 5, 7, 8)] |>
      setNames(c("year", "obs", "model", "lower", "upper")) |>
      mutate(fleet = fleet_name)
  })
}

# Mean age – surveys
extr_meanage_srv <- function(mod, stock = 1) {
  out      <- .jjm_out(mod, stock)
  sn       <- .snames(out)
  eff_cols <- grep("^EffN_Survey_", names(out), value = TRUE)
  purrr::imap_dfr(eff_cols, \(cn, i) {
    df <- as.data.frame(out[[cn]])
    if (nrow(df) <= 1 || ncol(df) < 8) return(NULL)
    df[, c(1, 4, 5, 7, 8)] |>
      setNames(c("year", "obs", "model", "lower", "upper")) |>
      mutate(survey = sn[i])
  })
}

# Mean length – fisheries
extr_meanlen_fsh <- function(mod, stock = 1) {
  out      <- .jjm_out(mod, stock)
  fn       <- .fnames(out)
  eff_cols <- grep("^EffN_Length_Fsh_", names(out), value = TRUE)
  if (!length(eff_cols)) return(tibble(year = integer(), obs = numeric(),
                                      model = numeric(), lower = numeric(),
                                      upper = numeric(), fleet = character()))
  eff_nums <- sort(as.integer(sub("^EffN_Length_Fsh_", "", eff_cols)))
  purrr::imap_dfr(eff_cols, \(cn, i) {
    fleet_num  <- as.integer(sub("^EffN_Length_Fsh_", "", cn))
    fleet_name <- if (length(fn) >= max(eff_nums)) fn[fleet_num] else fn[match(fleet_num, eff_nums)]
    df <- as.data.frame(out[[cn]])
    if (ncol(df) < 8) return(NULL)
    df[, c(1, 4, 5, 7, 8)] |>
      setNames(c("year", "obs", "model", "lower", "upper")) |>
      mutate(fleet = fleet_name)
  })
}

# Helper: pivot a [years × fleets] data matrix to long format
# rownames(mat) are year strings; names_vec supplies fleet/survey labels
.mat_long <- function(mat, names_vec, type_label, value_col) {
  if (is.null(mat)) return(NULL)
  years <- as.integer(rownames(mat))
  purrr::imap_dfr(seq_len(min(ncol(mat), length(names_vec))), \(j, ...) {
    tibble(year = years, !!value_col := mat[, j],
           name = names_vec[j], type = type_label)
  })
}

# Sample sizes from $data (Fagesample, Flengthsample, Iagesample)
extr_effn <- function(mod, stock = 1) {
  dat <- mod[[1]]$data
  out <- .jjm_out(mod, stock)
  fn  <- .fnames(out)
  sn  <- .snames(out)

  bind_rows(
    .mat_long(dat$Fagesample,    fn, "Fishery (age)",    "input_n"),
    .mat_long(dat$Flengthsample, fn, "Fishery (length)", "input_n"),
    .mat_long(dat$Iagesample,    sn, "Survey (age)",     "input_n")
  ) |> filter(!is.na(input_n), input_n > 0)
}

# Catch observation CV from $data$Fcatonerr
extr_catch_cv <- function(mod, stock = 1) {
  dat <- mod[[1]]$data
  out <- .jjm_out(mod, stock)
  fn  <- .fnames(out)
  if (is.null(dat$Fcatonerr))
    return(tibble(year = integer(), cv = numeric(), fleet = character()))
  .mat_long(dat$Fcatonerr, fn, "catch", "cv") |>
    select(year, cv, fleet = name) |>
    filter(!is.na(cv), cv > 0)
}

# Fished / unfished total biomass
extr_fished_unfished <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)
  bind_rows(
    data.frame(year = out$TotBiom[, 1],        bio = out$TotBiom[, 2],        scenario = "Fished"),
    data.frame(year = out$TotBiom_NoFish[, 1], bio = out$TotBiom_NoFish[, 2], scenario = "Unfished")
  )
}

# Retrospective data (works for SSB, R; "F" averages across all F_fsh_* fleets)
extr_retro <- function(retro, var = "SSB", stock = 1) {
  stk <- retro[[paste0("Stock_", stock)]]

  # Special case: "F" → average column 1 across all F_fsh_* fleet arrays
  if (var == "F") {
    fkeys <- grep("^F_fsh_", names(stk), value = TRUE)
    if (length(fkeys) == 0L) {
      warning("No F_fsh_* keys found in retro for stock ", stock)
      return(tibble(year = integer(), est = numeric(), lower = numeric(),
                    upper = numeric(), peel = integer()))
    }
    ref  <- stk[[fkeys[1]]]
    time <- ref$time
    n_peel <- dim(ref$var)[3]
    purrr::map_dfr(seq_len(n_peel), \(i) {
      est_mat <- sapply(fkeys, \(k) stk[[k]]$var[, 1, i])
      tibble(year = time, est = rowMeans(est_mat), lower = NA_real_,
             upper = NA_real_, peel = i - 1L)
    }) |> filter(!is.na(est))
  } else {
    dat <- stk[[var]]
    if (is.null(dat)) {
      warning("No retrospective data for var '", var, "' in stock ", stock)
      return(tibble(year = integer(), est = numeric(), lower = numeric(),
                    upper = numeric(), peel = integer()))
    }
    v <- dat$var
    d <- dim(v)
    if (!is.null(d) && length(d) == 3L) {
      n_run  <- d[[3]]
      has_ci <- d[[2]] >= 4L
      purrr::map_dfr(seq_len(n_run), \(i)
        tibble(year  = dat$time, est = v[, 1, i],
               lower = if (has_ci) v[, 3, i] else NA_real_,
               upper = if (has_ci) v[, 4, i] else NA_real_,
               peel  = i - 1L)
      )
    } else if (is.list(v)) {
      purrr::imap_dfr(v, \(mat, i) {
        has_ci <- ncol(mat) >= 4L
        tibble(year = dat$time, est = mat[, 1],
               lower = if (has_ci) mat[, 3] else NA_real_,
               upper = if (has_ci) mat[, 4] else NA_real_,
               peel  = i - 1L)
      })
    } else {
      if (is.null(d)) v <- as.matrix(v)
      has_ci <- ncol(v) >= 4L
      tibble(year = dat$time, est = v[, 1],
             lower = if (has_ci) v[, 3] else NA_real_,
             upper = if (has_ci) v[, 4] else NA_real_,
             peel  = 0L)
    } |> filter(!is.na(est))
  }
}

# Q-breaks: tibble(survey, break_year) from CTL random-walk q specification
extr_q_breaks <- function(mod) {
  ctl    <- mod[[1]]$control
  phases <- ctl$RW_q_phases
  nyrs   <- ctl$RW_nyrs_q
  yrs    <- ctl$RW_q_yrs
  snames <- .snames(.jjm_out(mod, 1))
  if (is.null(phases) || is.null(nyrs)) return(tibble(survey = character(), break_year = integer()))
  pos <- 1L
  purrr::imap_dfr(seq_along(phases), \(i, ...) {
    n <- nyrs[i]
    if (n == 0L) { pos <<- pos; return(NULL) }
    bk <- yrs[pos:(pos + n - 1L)]
    pos <<- pos + n
    tibble(survey = snames[i], break_year = as.integer(bk))
  })
}

# Input CV of survey indices: tibble(year, cv, survey)
# Derived from extr_indices(): cv = sd / obs (normalisation cancels out)
extr_cv <- function(mod, stock = 1) {
  df <- extr_indices(mod, stock)
  if (nrow(df) == 0)
    return(tibble(year = numeric(), cv = numeric(), survey = character()))
  df |>
    filter(obs > 0, !is.na(sd), sd > 0) |>
    mutate(cv = sd / obs) |>
    filter(is.finite(cv)) |>
    select(year, cv, survey)
}

# Selectivity-at-age extractor — fisheries
# Matrix layout: col1 = fleet index, col2 = year, col3+ = sel at age 1..n
extr_sel_fsh <- function(mod, stock = 1) {
  out  <- .jjm_out(mod, stock)
  fn   <- .fnames(out)
  cols <- grep("^sel_fsh_", names(out), value = TRUE)
  purrr::imap_dfr(cols, \(cn, i) {
    mat  <- as.matrix(out[[cn]])
    vals <- mat[, -c(1, 2), drop = FALSE]
    as.data.frame(vals) |>
      setNames(paste0("a", seq_len(ncol(vals)))) |>
      mutate(year = mat[, 2], fleet = fn[min(i, length(fn))]) |>
      pivot_longer(starts_with("a"), names_to = "age", values_to = "sel",
                   names_transform = list(age = \(x) as.integer(sub("a", "", x))))
  })
}

# Selectivity-at-age extractor — surveys
# Matrix layout: col1 = survey index, col2 = year, col3+ = sel at age 1..n
extr_sel_srv <- function(mod, stock = 1) {
  out  <- .jjm_out(mod, stock)
  sn   <- .snames(out)
  cols <- grep("^sel_ind_", names(out), value = TRUE)
  purrr::imap_dfr(cols, \(cn, i) {
    mat  <- as.matrix(out[[cn]])
    vals <- mat[, -c(1, 2), drop = FALSE]
    as.data.frame(vals) |>
      setNames(paste0("a", seq_len(ncol(vals)))) |>
      mutate(year = mat[, 2], survey = sn[min(i, length(sn))]) |>
      pivot_longer(starts_with("a"), names_to = "age", values_to = "sel",
                   names_transform = list(age = \(x) as.integer(sub("a", "", x))))
  })
}

# Extract selectivity block break years — returns empty tibble if RW (non-integer sigma)
extr_sel_breaks <- function(mod, type = c("fsh", "srv")) {
  type      <- match.arg(type)
  ctl       <- mod[[1]]$control
  out       <- .jjm_out(mod, 1)
  nms       <- if (type == "fsh") .fnames(out) else .snames(out)
  prefix    <- if (type == "fsh") "F" else "I"
  col_name  <- if (type == "fsh") "fleet" else "survey"
  purrr::imap_dfr(seq_along(nms), \(i, ...) {
    yrs <- ctl[[paste0(prefix, i, "_selchangeYear")]]
    chs <- ctl[[paste0(prefix, i, "_selchange")]]
    if (is.null(yrs) || length(yrs) == 0) return(NULL)
    if (any(abs(chs - round(chs)) > 1e-9, na.rm = TRUE)) return(NULL)
    tibble(!!col_name := nms[i], break_year = as.integer(yrs))
  })
}

# ---------------------------------------------------------------------------
# Guard helper: returns TRUE if a composition df has observed rows for filter
# ---------------------------------------------------------------------------
has_comp <- function(df, grp_var = NULL, grp_filter = NULL) {
  if (nrow(df) == 0 || !"type" %in% names(df)) return(FALSE)
  if (!is.null(grp_filter) && !is.null(grp_var))
    df <- df[df[[grp_var]] == grp_filter, ]
  nrow(df[df$type == "observed" & !is.na(df$prop), ]) > 0
}

# ---------------------------------------------------------------------------
# Figure-height helpers (use in chunk #| fig-height: !expr ...)
# ---------------------------------------------------------------------------

# Height for comp-fits patchwork (3 rows: year facets | bubble | marginals)
# ncol: assumed panels per row in the year-facet grid
comp_fig_height <- function(df, grp_var = NULL, grp_filter = NULL,
                             ncol = 5, panel_h = 2.2, row2_h = 2.5, row3_h = 2.2) {
  if (!is.null(grp_filter) && !is.null(grp_var))
    df <- df[df[[grp_var]] == grp_filter, ]
  n_yr <- length(unique(df$year[df$type == "observed" & !is.na(df$prop)]))
  if (n_yr == 0L) return(4)
  ceiling(n_yr / ncol) * panel_h + row2_h + row3_h
}

# Height for a simple facet plot given the number of panels and panels per row
facet_fig_height <- function(n_panels, ncol = 2, panel_h = 3.5, min_h = 4) {
  max(min_h, ceiling(n_panels / ncol) * panel_h)
}

# ---------------------------------------------------------------------------
# Plot functions
# ---------------------------------------------------------------------------

# Stacked area: catch by fleet + optional catch CV panel
plot_catch_fleet <- function(mod, title = NULL) {
  df  <- extr_catch(mod)
  nf  <- nlevels(df$fleet)
  pal <- setNames(FLEET_PAL[seq_len(nf)], levels(df$fleet))

  p_catch <- ggplot(df, aes(year, catch / 1e3, fill = fleet)) +
    geom_area(position = "stack", colour = "white", linewidth = 0.25, alpha = 0.9) +
    scale_fill_manual(values = pal, name = NULL) +
    scale_y_continuous(labels = label_comma()) +
    labs(x = NULL, y = "Catch (thousand mt)", title = title) +
    theme_jjm() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())

  df_cv <- extr_catch_cv(mod)
  if (nrow(df_cv) == 0) return(p_catch + theme(axis.text.x = element_text()))

  p_cv <- ggplot(df_cv, aes(year, cv, colour = fleet)) +
    geom_line(linewidth = 0.8) +
    geom_point(size = 1.5) +
    scale_colour_manual(values = pal, name = NULL) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    labs(x = NULL, y = "Catch CV") +
    theme_jjm() +
    theme(legend.position = "none")

  p_catch / p_cv + plot_layout(heights = c(3, 1))
}

# Mohn's rho over n_peel peels (Hurtado-Ferro et al. 2015)
calc_mohn_rho <- function(df, n_peel = 5) {
  max_yr <- max(df$year[df$peel == 0L])
  peels  <- seq_len(min(n_peel, max(df$peel)))
  rhos   <- purrr::map_dbl(peels, \(p) {
    yr       <- max_yr - p
    est_peel <- df$est[df$peel == p  & df$year == yr]
    est_base <- df$est[df$peel == 0L & df$year == yr]
    if (!length(est_peel) || !length(est_base) || est_base == 0) return(NA_real_)
    (est_peel - est_base) / est_base
  })
  mean(rhos, na.rm = TRUE)
}

# Retrospective: spaghetti plot (one line per peel), PFA colour scheme
plot_retro <- function(retro, var = "SSB", stock = 1, title = NULL) {
  df <- extr_retro(retro, var, stock)
  if (nrow(df) == 0L)
    return(ggplot() + theme_void() +
           labs(title = paste("No retrospective data for stock", stock)))
  n_peel   <- max(df$peel)
  max_yr   <- max(df$year)
  term_yrs <- setNames(as.character(max_yr - seq_len(n_peel)),
                       as.character(seq_len(n_peel)))
  lbl_map  <- c("0" = as.character(max_yr), term_yrs)
  # peel 0 = PFA_BLUE (terminal); each older peel gets a distinct colour
  pal <- c("0" = PFA_BLUE,
           setNames(.PEEL_PAL[seq_len(n_peel)], as.character(seq_len(n_peel))))
  ylab <- switch(var,
    SSB = "SSB (thousand mt)",
    R   = "Recruitment (millions)",
    F   = "Fishing mortality",
    var
  )
  rho     <- calc_mohn_rho(df, n_peel = 5)
  rho_lbl <- if (is.na(rho)) NULL else
             paste0("Mohn's ρ (5-yr) = ", sprintf("%+.3f", rho))

  df_ci <- filter(df, peel == 0L, !is.na(lower), !is.na(upper))

  p <- ggplot(df, aes(year, est / 1e3, group = peel, colour = factor(peel)))

  if (nrow(df_ci) > 0)
    p <- p + geom_ribbon(data = df_ci,
                         aes(x = year, ymin = lower / 1e3, ymax = upper / 1e3,
                             fill = factor(peel)),
                         alpha = 0.18, colour = NA, inherit.aes = FALSE)

  p +
    geom_line(aes(linewidth = peel == 0)) +
    scale_colour_manual(values = pal, labels = lbl_map, name = "Terminal year") +
    scale_fill_manual(values = pal, guide = "none") +
    scale_linewidth_manual(values = c("TRUE" = 1.6, "FALSE" = 0.9), guide = "none") +
    scale_x_continuous(breaks = breaks_width(5)) +
    labs(x = NULL, y = ylab, title = title, subtitle = rho_lbl) +
    theme_jjm()
}

# Weight-at-age line plot: one line per age, x = year, y = weight
plot_wtat <- function(df, facet_var, title = NULL, years = NULL) {
  if (is.null(years)) years <- seq(max(df$year, na.rm = TRUE) - 14, max(df$year, na.rm = TRUE))
  df <- filter(df, year %in% years)
  ggplot(df, aes(year, weight, group = age, colour = age)) +
    geom_line(linewidth = 0.9, alpha = 0.85) +
    scale_colour_gradient(low = PFA_BLUE, high = PFA_ORANGE,
                          name = "Age", breaks = breaks_width(2),
                          guide = guide_colourbar(barwidth = 16, barheight = 0.7)) +
    scale_x_continuous(breaks = breaks_width(5)) +
    facet_wrap(as.formula(paste("~", facet_var)), ncol = 2) +
    labs(x = NULL, y = "Weight (kg)", title = title) +
    theme_jjm()
}

# Weight-at-age cohort plot: one line per cohort (cohort = year - age), x = year
plot_wtat_cohort <- function(df, facet_var, title = NULL, years = NULL) {
  if (is.null(years)) years <- seq(max(df$year, na.rm = TRUE) - 14, max(df$year, na.rm = TRUE))
  df <- df |>
    filter(year %in% years) |>
    mutate(cohort = year - age) |>
    filter(!is.na(weight))
  ggplot(df, aes(year, weight, group = cohort, colour = cohort)) +
    geom_line(linewidth = 0.9, alpha = 0.85) +
    scale_colour_gradient(low = PFA_BLUE, high = PFA_ORANGE,
                          name = "Cohort", breaks = breaks_width(10),
                          guide = guide_colourbar(barwidth = 16, barheight = 0.7)) +
    scale_x_continuous(breaks = breaks_width(5)) +
    facet_wrap(as.formula(paste("~", facet_var)), ncol = 2) +
    labs(x = NULL, y = "Weight (kg)", title = title) +
    theme_jjm()
}

# Age / length composition fits — 3-row patchwork: fits | bubble residuals | marginal residuals
plot_comp_fits <- function(df, x_var, grp_var, grp_filter = NULL,
                           x_lab = "Age", title = NULL) {
  if (nrow(df) == 0 || !"type" %in% names(df))
    return(ggplot() + theme_void() +
           labs(title = title, subtitle = "No composition data available"))

  if (!is.null(grp_filter))
    df <- df[df[[grp_var]] == grp_filter, ]

  obs  <- filter(df, type == "observed",  !is.na(prop))
  pred <- filter(df, type == "predicted", !is.na(prop))

  if (nrow(obs) == 0)
    return(ggplot() + theme_void() +
           labs(title = title, subtitle = "No composition data available"))

  # Row 1: proportion fits faceted by year
  p_fit <- ggplot() +
    geom_point(data = obs,  aes(.data[[x_var]], prop),
               shape = 19, colour = PFA_BLUE, size = 1.1) +
    geom_line(data = pred,  aes(.data[[x_var]], prop),
              colour = PFA_ORANGE, linewidth = 0.8) +
    facet_wrap(~year, scales = "fixed") +
    labs(x = x_lab, y = "Proportion", title = title) +
    theme_jjm() +
    theme(strip.text    = element_text(size = 6),
          axis.text     = element_text(size = 6),
          panel.spacing = unit(0.15, "lines"))

  dw <- left_join(
    obs  |> select(year, all_of(x_var), obs_prop  = prop),
    pred |> select(year, all_of(x_var), pred_prop = prop),
    by = c("year", x_var)
  ) |> mutate(resid = obs_prop - pred_prop,
              sign  = if_else(resid >= 0, "Positive", "Negative"))

  # Row 2: bubble residuals on year × age grid
  p_resid <- ggplot(dw, aes(year, .data[[x_var]], size = abs(resid), colour = sign)) +
    geom_point(alpha = 0.75) +
    scale_colour_manual(values = c(Positive = PFA_ORANGE, Negative = PFA_BLUE), name = NULL) +
    scale_size_area(max_size = 6, name = "|residual|",
                    guide = guide_legend(override.aes = list(colour = "grey50"))) +
    scale_x_continuous(breaks = breaks_width(5)) +
    scale_y_continuous(breaks = breaks_width(2)) +
    labs(x = NULL, y = x_lab) +
    theme_jjm()

  # Row 3: marginal residuals by age (left) and by year (right)
  p_age <- ggplot(dw, aes(factor(.data[[x_var]]), resid)) +
    geom_hline(yintercept = 0, colour = "grey40", linetype = "dashed") +
    geom_boxplot(fill = PFA_BLUE, alpha = 0.7, outlier.size = 0.8) +
    labs(x = x_lab, y = "obs – pred") +
    theme_jjm()

  p_yr <- ggplot(dw, aes(factor(year), resid)) +
    geom_hline(yintercept = 0, colour = "grey40", linetype = "dashed") +
    geom_boxplot(fill = PFA_ORANGE, alpha = 0.7, outlier.size = 0.8) +
    labs(x = "Year", y = NULL) +
    theme_jjm() +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, size = 6))

  p_fit / p_resid / (p_age | p_yr) + plot_layout(heights = c(4, 1.5, 1.5))
}

# Survey index fits with q-breaks and input CV panel (patchwork, 3:1 height ratio)
plot_indices_v2 <- function(mod, stock = 1, title = NULL) {
  df_fit  <- extr_indices(mod, stock)
  df_cv   <- extr_cv(mod, stock)
  df_brk  <- extr_q_breaks(mod)

  add_breaks <- function(p) {
    if (nrow(df_brk) == 0) return(p)
    df_b <- filter(df_brk, survey %in% unique(df_fit$survey))
    if (nrow(df_b) == 0) return(p)
    p +
      geom_vline(data = df_b,
                 aes(xintercept = break_year),
                 colour = PFA_ORANGE, linetype = "dashed", linewidth = 0.6) +
      geom_text(data = df_b,
                aes(x = break_year, y = Inf, label = break_year),
                angle = 90, vjust = -0.3, hjust = 1.1,
                colour = PFA_ORANGE, size = 3, inherit.aes = FALSE)
  }

  df_obs <- filter(df_fit, !is.na(obs), obs > 0)

  p_fit <- ggplot(df_fit, aes(year)) +
    geom_line(aes(y = model),  colour = PFA_ORANGE,  linewidth = 1.1) +
    geom_errorbar(data = df_obs,
                  aes(ymin = pmax(obs - 1.96 * sd, 0), ymax = obs + 1.96 * sd),
                  colour = PFA_BLUE, width = 0.4, linewidth = 0.5) +
    geom_point(data = df_obs, aes(y = obs), colour = PFA_BLUE, size = 2) +
    facet_wrap(~survey, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = breaks_width(5)) +
    labs(x = NULL, y = "Standardised index (relative)") +
    theme_jjm() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  p_fit <- add_breaks(p_fit)

  p_cv <- ggplot(df_cv, aes(year, cv)) +
    geom_area(fill = PFA_BLUE2, alpha = 0.45) +
    geom_line(colour = PFA_BLUE, linewidth = 0.7) +
    facet_wrap(~survey, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = breaks_width(5)) +
    scale_y_continuous(labels = label_percent(accuracy = 1)) +
    labs(x = "Year", y = "Input CV") +
    theme_jjm() +
    theme(strip.text = element_blank(),
          axis.text.x = element_text(angle = 45, hjust = 1))
  p_cv <- add_breaks(p_cv)

  if (nrow(df_cv) == 0)
    return(
      p_fit +
        scale_x_continuous(breaks = breaks_width(5)) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1),
              axis.ticks.x = element_line()) +
        labs(x = "Year") +
        plot_annotation(title = title)
    )

  p_fit / p_cv +
    plot_layout(heights = c(3, 1)) +
    plot_annotation(title = title)
}

# Input composition ridge plot – observed proportions over time
# df      : output of extr_agefits_* or extr_lenfits_* (contains type, prop)
# x_var   : "age" or "length_bin"
# facet_var: "fleet" or "survey"
plot_comp_ridge <- function(df, x_var = "age", facet_var = "fleet",
                             x_lab = "Age", title = NULL) {
  obs <- filter(df, type == "observed", !is.na(prop), prop > 0)
  if (nrow(obs) == 0)
    return(ggplot() + theme_void() + labs(title = paste0(title, " — no data")))

  obs <- obs |>
    group_by(across(all_of(c(facet_var, "year")))) |>
    mutate(prop = prop / max(prop, na.rm = TRUE)) |>
    ungroup()

  ggplot(obs, aes(x = .data[[x_var]], y = year, height = prop,
                   group = year, fill = year)) +
    geom_ridgeline(scale = 2, alpha = 0.85, colour = PFA_BLUE, linewidth = 0.25) +
    scale_fill_gradient(low = PFA_BLUE, high = PFA_ORANGE) +
    scale_y_reverse(breaks = breaks_width(5)) +
    scale_x_continuous(breaks = breaks_width(2)) +
    facet_wrap(as.formula(paste("~", facet_var)), nrow = 1) +
    labs(x = x_lab, y = "Year", title = title) +
    guides(fill = "none") +
    theme_jjm()
}

# Selectivity ridge plot (fisheries and surveys)
# breaks_df: tibble(fleet/survey, break_year) from extr_sel_breaks(); NULL = no lines
plot_sel_ridge <- function(df, facet_var, title = NULL, breaks_df = NULL) {
  if (nrow(df) == 0)
    return(ggplot() + theme_void() +
           labs(title = paste0(title, " — selectivity data not found")))
  df <- df |>
    group_by(across(all_of(c(facet_var, "year")))) |>
    mutate(sel = sel / max(sel, na.rm = TRUE)) |>
    ungroup()
  p <- ggplot(df, aes(x = age, y = year, height = sel, group = year, fill = year)) +
    geom_ridgeline(scale = 2, alpha = 0.85, colour = PFA_BLUE, linewidth = 0.25) +
    scale_fill_gradient(low = PFA_BLUE, high = PFA_ORANGE) +
    scale_y_reverse(breaks = breaks_width(5)) +
    scale_x_continuous(breaks = breaks_width(2)) +
    facet_wrap(as.formula(paste("~", facet_var)), nrow = 1) +
    labs(x = "Age", y = "Year", title = title) +
    guides(fill = "none") +
    theme_jjm()
  if (!is.null(breaks_df) && nrow(breaks_df) > 0 && facet_var %in% names(breaks_df))
    p <- p + geom_hline(data = breaks_df, aes(yintercept = break_year),
                        colour = "white", linetype = "dashed", linewidth = 0.7,
                        inherit.aes = FALSE)
  p
}

# 6-panel fit diagnostics: time series | 1:1 | residuals over time | Tukey-Anscombe | Q-Q | ACF
# break_years: integer/numeric vector of q-break years; adds orange dashed vlines to panels a & c
plot_fit_diagnostics <- function(df, title = NULL, break_years = NULL) {
  has_sd <- "sd" %in% names(df) && !all(is.na(df$sd))
  df <- df |>
    filter(!is.na(obs), !is.na(model), obs > 0, model > 0) |>
    mutate(std_resid = if (has_sd) (obs - model) / sd else obs - model)
  resid_lab <- if (has_sd) "Standardised residuals" else "Residuals"

  p_ts <- ggplot(df, aes(year)) +
    geom_line(aes(y = model),  colour = PFA_ORANGE, linewidth = 0.8) +
    geom_point(aes(y = model), colour = PFA_ORANGE, shape = 4,  size = 1.8) +
    { if (has_sd) geom_errorbar(aes(ymin = pmax(obs - 1.96 * sd, 1e-6),
                                    ymax = obs + 1.96 * sd),
                                colour = PFA_BLUE, width = 0.4, linewidth = 0.5) } +
    geom_point(aes(y = obs),   colour = PFA_BLUE,   shape = 19, size = 1.8) +
    scale_y_log10(labels = label_comma()) +
    labs(x = "Year", y = "Values", title = "a) Observed and fitted") +
    theme_jjm()
  if (!is.null(break_years) && length(break_years) > 0) {
    p_ts <- p_ts +
      geom_vline(xintercept = break_years, colour = PFA_ORANGE,
                 linetype = "dashed", linewidth = 0.5) +
      annotate("text", x = break_years, y = Inf, label = break_years,
               angle = 90, vjust = -0.3, hjust = 1.1,
               colour = PFA_ORANGE, size = 3)
  }

  lim <- range(c(df$obs, df$model), na.rm = TRUE)
  p_1to1 <- ggplot(df, aes(model, obs)) +
    geom_abline(slope = 1, intercept = 0, linewidth = 0.8) +
    geom_point(colour = PFA_BLUE, size = 1.8) +
    scale_x_log10(limits = lim) +
    scale_y_log10(limits = lim) +
    labs(x = "Fitted", y = "Observed", title = "b) Observed vs fitted") +
    theme_jjm()

  p_res_ts <- ggplot(df, aes(year, std_resid)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_segment(aes(xend = year, yend = 0), linewidth = 0.6) +
    geom_point(colour = PFA_BLUE, size = 1.8) +
    labs(x = "Year", y = resid_lab, title = "c) Residuals over time") +
    theme_jjm()
  if (!is.null(break_years) && length(break_years) > 0) {
    p_res_ts <- p_res_ts +
      geom_vline(xintercept = break_years, colour = PFA_ORANGE,
                 linetype = "dashed", linewidth = 0.5)
  }

  p_ta <- ggplot(df, aes(model, std_resid)) +
    geom_hline(yintercept = 0, colour = "grey50") +
    geom_point(colour = PFA_BLUE, size = 1.8) +
    scale_x_log10() +
    labs(x = "Fitted (log scale)", y = resid_lab, title = "d) Tukey-Anscombe") +
    theme_jjm()

  p_qq <- ggplot(df, aes(sample = std_resid)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey40") +
    stat_qq_line(colour = PFA_ORANGE, linewidth = 0.9) +
    stat_qq(colour = PFA_BLUE, size = 1.8) +
    labs(x = "Theoretical quantiles", y = resid_lab, title = "e) Normal Q-Q") +
    theme_jjm()

  r <- df$std_resid[!is.na(df$std_resid)]
  if (length(r) >= 4) {
    acf_out  <- acf(r, plot = FALSE)
    ci_bound <- qnorm(0.975) / sqrt(length(r))
    acf_df   <- tibble(lag = as.integer(acf_out$lag[-1]),
                       acf = as.numeric(acf_out$acf[-1]))
    p_acf <- ggplot(acf_df, aes(lag, acf)) +
      geom_hline(yintercept = 0, colour = "grey50") +
      geom_hline(yintercept = c(-ci_bound, ci_bound),
                 linetype = "dashed", colour = PFA_ORANGE, linewidth = 0.8) +
      geom_segment(aes(xend = lag, yend = 0), linewidth = 0.9, colour = PFA_BLUE) +
      annotate("text", x = Inf, y = ci_bound, label = "95% CI",
               hjust = 1.1, vjust = -0.4, colour = PFA_ORANGE, size = 3.5) +
      labs(x = "Lag (yrs)", y = "ACF", title = "f) Autocorrelation of residuals") +
      theme_jjm()
  } else {
    p_acf <- ggplot() + theme_void() +
      labs(title = "f) Autocorrelation (insufficient data)")
  }

  (p_ts | p_1to1) / (p_res_ts | p_ta) / (p_qq | p_acf) +
    plot_annotation(title = title)
}

# Sample sizes: bars = input N, points = effective N, faceted by fleet/survey
plot_effn <- function(mod, stock = 1, title = NULL) {
  df <- extr_effn(mod, stock)
  if (nrow(df) == 0)
    return(ggplot() + theme_void() +
           labs(title = title, subtitle = "No sample size data available"))
  ggplot(df, aes(year)) +
    geom_col(aes(y = input_n), fill = PFA_BLUE2, alpha = 0.65, width = 0.85) +
    facet_wrap(~ paste0(name, "\n(", type, ")"), scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = breaks_width(5),
                       guide = guide_axis(check.overlap = TRUE)) +
    labs(x = NULL, y = "Input sample size (N)", title = title) +
    theme_jjm() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# Mean age or mean length (obs + CI band + modelled line)
plot_meanstat <- function(df, facet_var, y_lab = "Mean age (years)", title = NULL) {
  if (nrow(df) == 0 || !facet_var %in% names(df))
    return(ggplot() + theme_void() +
           labs(title = title, subtitle = "No data available"))
  ggplot(df, aes(year)) +
    geom_errorbar(aes(ymin = lower, ymax = upper), colour = PFA_BLUE,
                  width = 0.4, linewidth = 0.5) +
    geom_point(aes(y = obs),   colour = PFA_BLUE,   size = 2) +
    geom_line(aes(y = model),  colour = PFA_ORANGE,  linewidth = 1.1) +
    facet_wrap(as.formula(paste("~", facet_var)), scales = "free_y", ncol = 2) +
    labs(x = NULL, y = y_lab, title = title) +
    theme_jjm()
}

# Fished vs unfished total biomass
plot_fished_unfished <- function(mod, stock = 1, title = NULL) {
  df <- extr_fished_unfished(mod, stock)
  ggplot(df, aes(year, bio / 1e3, linetype = scenario, colour = scenario)) +
    geom_line(linewidth = 1.3) +
    scale_linetype_manual(values = c("Fished" = "solid", "Unfished" = "dashed")) +
    scale_colour_manual(values  = c("Fished" = PFA_BLUE, "Unfished" = PFA_ORANGE)) +
    labs(x = NULL, y = "Total biomass (thousand mt)", linetype = NULL, colour = NULL,
         title = title) +
    theme_jjm()
}

# Summary sheet (SSB / F / Recruitment / Catch – 2×2 patchwork)
plot_summary_sheet <- function(mod, stock = 1, title = NULL) {
  out <- .jjm_out(mod, stock)
  yr  <- out$Yr
  msy <- out$msy_mt

  avg_bmsy <- mean(rev(msy[, 10])[1:min(10, nrow(msy))])

  ssb <- as.data.frame(out$SSB); colnames(ssb) <- c("year", "est", "sd", "lower", "upper")
  p_ssb <- ggplot(ssb, aes(year)) +
    geom_ribbon(aes(ymin = lower / 1e3, ymax = upper / 1e3),
                fill = PFA_BLUE, alpha = 0.2) +
    geom_line(aes(y = est / 1e3), linewidth = 1.3, colour = PFA_BLUE) +
    geom_line(data = data.frame(year = yr, bmsy = msy[, 10] / 1e3),
              aes(y = bmsy), colour = PFA_BLUE2, linewidth = 1, linetype = "dashed") +
    geom_hline(yintercept = avg_bmsy / 1e3, colour = PFA_ORANGE, linewidth = 1) +
    labs(x = NULL, y = "SSB (thousand mt)", title = "Spawning Biomass",
         subtitle = "Dashed = dynamic Bᴹₛʏ; orange = 10-yr avg") +
    theme_jjm()

  rec <- as.data.frame(out$R); colnames(rec) <- c("year", "est", "sd", "lower", "upper")
  p_rec <- ggplot(rec, aes(year)) +
    geom_col(aes(y = est / 1e6), fill = PFA_BLUE2, width = 0.8, alpha = 0.8) +
    geom_errorbar(aes(ymin = lower / 1e6, ymax = upper / 1e6),
                  width = 0, linewidth = 0.6, colour = PFA_BLUE) +
    labs(x = NULL, y = "Recruitment (billions)", title = "Recruitment") +
    theme_jjm()

  f_df <- data.frame(year = yr, f = rowMeans(out$TotF[, -1, drop = FALSE]),
                     fmsy = msy[, 5])
  p_f <- ggplot(f_df, aes(year, f)) +
    geom_line(linewidth = 1.3, colour = PFA_BLUE) +
    geom_line(aes(y = fmsy), colour = PFA_BLUE2, linewidth = 1, linetype = "dashed") +
    labs(x = NULL, y = "Fishing mortality", title = "Fishing Mortality",
         subtitle = "Dashed = dynamic Fᴹₛʏ") +
    theme_jjm()

  catch_cols <- grep("^Obs_catch_", names(out), value = TRUE)
  tot_catch  <- rowSums(sapply(catch_cols, \(c) out[[c]]))
  p_catch <- ggplot(data.frame(year = yr, catch = tot_catch / 1e3),
                    aes(year, catch)) +
    geom_col(fill = PFA_ORANGE2, width = 0.8, alpha = 0.85) +
    labs(x = NULL, y = "Catch (thousand mt)", title = "Total Catch") +
    theme_jjm()

  # Index log-residuals panel (5th panel, full width)
  df_ir <- extr_indices(mod, stock) |>
    filter(!is.na(obs), obs > 0, model > 0) |>
    mutate(log_resid = log(obs / model))
  sn_ir  <- unique(df_ir$survey)
  pal_ir <- setNames(SURV_PAL[seq_along(sn_ir)], sn_ir)
  p_idx  <- ggplot(df_ir, aes(year, log_resid, colour = survey)) +
    geom_hline(yintercept = 0, colour = "grey40", linetype = "dashed") +
    geom_point(size = 1.5, alpha = 0.8) +
    geom_line(linewidth = 0.5, alpha = 0.6) +
    scale_colour_manual(values = pal_ir, name = NULL) +
    labs(x = NULL, y = "log(obs/pred)", title = "Index log-residuals") +
    theme_jjm() +
    theme(legend.position = "right", legend.text = element_text(size = 8))

  (p_ssb | p_f) / (p_rec | p_catch) / p_idx +
    plot_layout(heights = c(3, 3, 2)) +
    plot_annotation(title = title)
}

# ---------------------------------------------------------------------------
# Stock-recruitment  (items 4)
# ---------------------------------------------------------------------------

# Extract SR pairs (year, ssb, r) and fitted B-H curve from model output.
# Stock_Rec cols: year | B0_ref | SSB | R  (all in model-native units)
# stock_Rec_Curve_1: SSB_axis | R_axis
extr_sr <- function(mod, stock = 1) {
  out   <- .jjm_out(mod, stock)
  sr    <- as.data.frame(out$Stock_Rec)
  curve <- as.data.frame(out$stock_Rec_Curve_1)
  steep <- out$Steep
  h     <- if (length(steep) >= 2) steep[2] else NA_real_

  pairs <- tibble(
    year = as.integer(sr[, 1]),
    ssb  = sr[, 3],
    r    = sr[, 4]
  ) |> filter(!is.na(ssb), !is.na(r), ssb > 0, r > 0)

  curve_df <- tibble(ssb = curve[, 1], r = curve[, 2]) |>
    filter(ssb > 0, r > 0)

  list(pairs = pairs, curve = curve_df, steepness = h)
}

plot_sr <- function(mod, stock = 1, title = NULL) {
  obj  <- extr_sr(mod, stock)
  df   <- obj$pairs
  crv  <- obj$curve
  h    <- obj$steepness
  n    <- nrow(df)
  if (n == 0)
    return(ggplot() + theme_void() + labs(title = "No SR data"))

  h_lbl <- if (!is.na(h)) paste0("h = ", round(h, 3)) else NULL

  ggplot(df, aes(ssb, r)) +
    geom_line(data = crv, aes(ssb, r), colour = PFA_ORANGE,
              linewidth = 1.1, inherit.aes = FALSE) +
    geom_point(aes(colour = year), size = 2.2, alpha = 0.85) +
    geom_text(data = slice(df, c(1L, n)), aes(label = year),
              vjust = -0.8, size = 3, colour = PFA_BLUE) +
    scale_colour_gradient(low = PFA_BLUE, high = PFA_ORANGE2, name = "Year") +
    guides(colour = guide_colourbar(barwidth = unit(8, "cm"), barheight = unit(0.4, "cm"),
                                    title.position = "top", title.hjust = 0.5)) +
    labs(x = "SSB", y = "Recruitment", title = title %||% "Stock-recruitment",
         subtitle = h_lbl) +
    theme_jjm()
}

# ---------------------------------------------------------------------------
# Kobe / phase plot  (item 2)
# ---------------------------------------------------------------------------

# msy_mt column layout (confirmed from output inspection):
# 1=year, 2=fspr, 3=survivespr, 4=f_fmsy, 5=fmsy, 6=f,
# 7=fsprmsy, 8=msy, 9=msyl, 10=bmsy, 11=bzero, 12=ssb, 13=b_bmsy
extr_kobe <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)
  msy <- out$msy_mt
  tibble(
    year  = as.integer(msy[, 1]),
    bbmsy = msy[, 13],
    ffmsy = msy[, 4]
  )
}

plot_kobe <- function(mod, stock = 1, title = NULL) {
  df <- extr_kobe(mod, stock)
  n  <- nrow(df)
  if (n == 0) return(ggplot() + theme_void() + labs(title = "No Kobe data"))

  xlim <- c(0, max(3, max(df$bbmsy, na.rm = TRUE) * 1.1))
  ylim <- c(0, max(3, max(df$ffmsy, na.rm = TRUE) * 1.1))

  ggplot(df, aes(bbmsy, ffmsy)) +
    # quadrant shading: red = overfished+overfishing, yellow = one criterion, green = safe
    annotate("rect", xmin = 0,    xmax = 1,       ymin = 1,    ymax = ylim[2],
             fill = "#FF3333", alpha = 0.15) +
    annotate("rect", xmin = 0,    xmax = 1,       ymin = 0,    ymax = 1,
             fill = "#FFD700", alpha = 0.15) +
    annotate("rect", xmin = 1,    xmax = xlim[2], ymin = 1,    ymax = ylim[2],
             fill = "#FFD700", alpha = 0.15) +
    annotate("rect", xmin = 1,    xmax = xlim[2], ymin = 0,    ymax = 1,
             fill = PFA_GREEN, alpha = 0.15) +
    geom_vline(xintercept = 1, colour = "grey50", linewidth = 0.5) +
    geom_hline(yintercept = 1, colour = "grey50", linewidth = 0.5) +
    geom_path(aes(colour = year), linewidth = 0.8) +
    geom_point(aes(colour = year), size = 1.8) +
    geom_point(data = slice_tail(df, n = 1), size = 4.5,
               colour = PFA_BLUE, shape = 18) +
    geom_text(data = slice(df, c(1L, n)),
              aes(label = year), vjust = -0.9, size = 3, colour = PFA_BLUE) +
    scale_colour_gradient(low = PFA_BLUE, high = PFA_ORANGE, name = "Year") +
    guides(colour = guide_colourbar(barwidth = unit(8, "cm"), barheight = unit(0.4, "cm"),
                                    title.position = "top", title.hjust = 0.5)) +
    coord_cartesian(xlim = xlim, ylim = ylim) +
    labs(x = "B / Bmsy", y = "F / Fmsy",
         title = title %||% "Kobe phase plot") +
    theme_jjm()
}

# ---------------------------------------------------------------------------
# Strong-cohort annotation  (item 5)
# ---------------------------------------------------------------------------

# Returns the top-n recruitment years (by model-estimated R)
strong_cohorts <- function(mod, stock = 1, n = 3) {
  out <- .jjm_out(mod, stock)
  rec <- as.data.frame(out$R)
  colnames(rec)[1:2] <- c("year", "est")
  rec |> arrange(desc(est)) |> slice_head(n = n) |> pull(year) |> as.integer()
}

# ggplot layer list — add to any plot with a year x-axis via `+ annotate_cohorts(...)`
annotate_cohorts <- function(years, colour = PFA_GREEN, label_size = 3) {
  list(
    geom_vline(xintercept = years, colour = colour,
               linetype = "dashed", linewidth = 0.5, alpha = 0.8),
    annotate("text", x = years, y = Inf,
             label = paste0("'", substr(as.character(years), 3, 4)),
             colour = colour, size = label_size, vjust = -0.3, hjust = -0.2),
    labs(caption = paste0("Green dashed lines = strong cohort years (",
                          paste(years, collapse = ", "), ")"))
  )
}

# ---------------------------------------------------------------------------
# Francis reweighting  (item 6)
# ---------------------------------------------------------------------------

# For each fleet/survey, compute Francis T1.8 effective N per year and compare
# to input N. Returns tibble(source, year, input_n, francis_n, ratio).
# agefits:  output of extr_agefits_fsh() or extr_agefits_srv()
# effn_df:  output of extr_effn(), pre-filtered to matching type
# grp_var:  "fleet" or "survey"
extr_francis <- function(agefits, effn_df, grp_var = "fleet") {
  obs  <- filter(agefits, type == "observed",  !is.na(prop))
  pred <- filter(agefits, type == "predicted", !is.na(prop))
  if (nrow(obs) == 0) return(tibble())

  inner_join(
    obs  |> select(year, age, all_of(grp_var), obs_p  = prop),
    pred |> select(year, age, all_of(grp_var), pred_p = prop),
    by = c("year", "age", grp_var)
  ) |>
    group_by(.data[[grp_var]], year) |>
    summarise(
      num   = sum(pred_p * (1 - pred_p), na.rm = TRUE),
      denom = sum((obs_p - pred_p)^2,    na.rm = TRUE),
      .groups = "drop"
    ) |>
    left_join(
      effn_df |> rename(!!grp_var := name, input_n = input_n),
      by = c(grp_var, "year")
    ) |>
    filter(!is.na(input_n), input_n > 0, denom > 0) |>
    mutate(francis_n = num / (denom / input_n),
           ratio     = francis_n / input_n) |>
    select(source = all_of(grp_var), year, input_n, francis_n, ratio)
}

plot_francis <- function(mod, stock = 1, title = NULL) {
  out  <- .jjm_out(mod, stock)
  fn   <- .fnames(out); sn <- .snames(out)
  en   <- extr_effn(mod, stock)

  fsh_age <- extr_agefits_fsh(mod, stock)
  srv_age <- extr_agefits_srv(mod, stock)

  df <- bind_rows(
    if (nrow(fsh_age) > 0)
      extr_francis(fsh_age, filter(en, type == "Fishery (age)"), "fleet"),
    if (nrow(srv_age) > 0)
      extr_francis(srv_age, filter(en, type == "Survey (age)"), "survey")
  )

  if (nrow(df) == 0)
    return(ggplot() + theme_void() + labs(title = "No composition data for Francis"))

  ggplot(df, aes(year)) +
    geom_col(aes(y = input_n), fill = PFA_BLUE2, alpha = 0.55, width = 0.85) +
    geom_point(aes(y = francis_n), colour = PFA_ORANGE, size = 1.8) +
    geom_hline(aes(yintercept = mean(input_n)), colour = PFA_BLUE,
               linetype = "dashed", linewidth = 0.6) +
    facet_wrap(~source, scales = "free_y", ncol = 2) +
    scale_x_continuous(breaks = breaks_width(5),
                       guide = guide_axis(check.overlap = TRUE)) +
    labs(x = NULL, y = "N  (bars = input, dots = Francis)",
         title = title %||% "Francis reweighting",
         subtitle = "Dashed = mean input N; dots above/below bars = under/over-weighted") +
    theme_jjm() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

# ---------------------------------------------------------------------------
# Q trajectories  (item 9)
# ---------------------------------------------------------------------------

# Builds a year-by-year q series for each survey using Index_Q_* (block values)
# and q-break years from extr_q_breaks().  Falls back to scalar Q_Survey_*
# estimate if no time-variation.
extr_q_trajectory <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)
  sn  <- .snames(out)
  # q_i is a 2-column matrix: col1 = year, col2 = q (one row per observed year)
  purrr::imap_dfr(seq_along(sn), \(i, ...) {
    qmat <- out[[paste0("q_", i)]]
    if (is.null(qmat) || !is.matrix(qmat) || ncol(qmat) < 2) return(NULL)
    tibble(year = as.integer(qmat[, 1]), q = qmat[, 2], survey = sn[i])
  })
}

plot_q_trajectory <- function(mod, stock = 1, title = NULL) {
  df <- extr_q_trajectory(mod, stock) |> filter(!is.na(q))
  if (nrow(df) == 0)
    return(ggplot() + theme_void() + labs(title = "No q data"))

  n_srv  <- length(unique(df$survey))
  pal_sv <- setNames(SURV_PAL[seq_len(n_srv)], unique(df$survey))

  ggplot(df, aes(year, q, colour = survey)) +
    geom_step(linewidth = 0.9) +
    scale_colour_manual(values = pal_sv, name = NULL) +
    scale_x_continuous(breaks = breaks_width(5)) +
    facet_wrap(~survey, scales = "free_y", ncol = 2) +
    labs(x = NULL, y = "Catchability (q)", title = title %||% "Survey catchability") +
    theme_jjm() +
    theme(legend.position = "none")
}

# ---------------------------------------------------------------------------
# Survey skill score — ROC curves  (item 11)
# Truth  = year-over-year direction of model SSB (1 = increase, 0 = decrease)
# Score  = year-over-year log-ratio of survey index (continuous, higher = more positive)
# AUC    = area under the ROC curve via trapezoidal rule
# ---------------------------------------------------------------------------

extr_survey_skill <- function(mod, stock = 1) {
  out <- .jjm_out(mod, stock)

  # SSB direction from msy_mt col 12 = ssb
  msy <- out$msy_mt
  ssb_df <- tibble(year = as.integer(msy[, 1]), ssb = msy[, 12]) |>
    arrange(year) |>
    mutate(ssb_dir = as.integer(ssb > lag(ssb))) |>
    filter(!is.na(ssb_dir))

  # Survey log-ratios
  ind_df <- extr_indices(mod, stock) |>
    filter(!is.na(obs), obs > 0) |>
    arrange(survey, year) |>
    group_by(survey) |>
    mutate(log_ratio = log(obs / lag(obs))) |>
    filter(!is.na(log_ratio)) |>
    ungroup()

  inner_join(ind_df, ssb_df |> select(year, ssb_dir), by = "year")
}

.calc_roc <- function(truth, score) {
  thresholds <- sort(unique(score), decreasing = TRUE)
  roc <- purrr::map_dfr(c(Inf, thresholds), \(thr) {
    pred <- as.integer(score >= thr)
    tp   <- sum(pred == 1L & truth == 1L)
    fp   <- sum(pred == 1L & truth == 0L)
    fn   <- sum(pred == 0L & truth == 1L)
    tn   <- sum(pred == 0L & truth == 0L)
    tibble(tpr = if (tp + fn > 0) tp / (tp + fn) else 0,
           fpr = if (fp + tn > 0) fp / (fp + tn) else 0)
  })
  bind_rows(tibble(tpr = 0, fpr = 0), roc, tibble(tpr = 1, fpr = 1)) |> distinct()
}

.calc_auc <- function(roc_df) {
  r <- arrange(roc_df, fpr, tpr)
  sum(diff(r$fpr) * (head(r$tpr, -1) + tail(r$tpr, -1)) / 2)
}

plot_survey_skill <- function(mod, stock = 1, title = NULL) {
  df <- extr_survey_skill(mod, stock)
  if (nrow(df) == 0)
    return(ggplot() + theme_void() + labs(title = "No survey skill data"))

  srvs <- unique(df$survey)
  n_srv <- length(srvs)
  pal   <- setNames(SURV_PAL[seq_len(n_srv)], srvs)

  roc_df <- purrr::map_dfr(srvs, \(s) {
    d   <- filter(df, survey == s)
    roc <- .calc_roc(d$ssb_dir, d$log_ratio)
    auc <- .calc_auc(roc)
    mutate(roc, survey = s,
           facet_lbl = sprintf("%s  |  AUC = %.2f  (n = %d)", s, auc, nrow(d)))
  })

  ggplot(roc_df, aes(fpr, tpr, colour = survey)) +
    geom_abline(slope = 1, intercept = 0, colour = "grey70", linetype = "dashed",
                linewidth = 0.5) +
    geom_step(linewidth = 0.9, direction = "hv") +
    geom_point(size = 1.8) +
    scale_colour_manual(values = pal, name = NULL) +
    scale_x_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    facet_wrap(~facet_lbl, ncol = 2) +
    labs(x = "False positive rate", y = "True positive rate",
         title = title %||% "Survey skill (ROC)",
         caption = "Truth = year-over-year direction of model SSB; score = survey log-ratio") +
    theme_jjm() +
    theme(legend.position = "none",
          strip.text = element_text(size = 8))
}
