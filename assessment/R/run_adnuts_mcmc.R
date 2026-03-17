#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(adnuts)
})

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || is.na(x)) {
    return(y)
  }
  x
}

parse_bool <- function(x) {
  x <- tolower(x)
  if (x %in% c("true", "t", "1", "yes", "y")) {
    return(TRUE)
  }
  if (x %in% c("false", "f", "0", "no", "n")) {
    return(FALSE)
  }
  stop("Could not parse logical value: ", x)
}

parse_int <- function(x, name) {
  out <- suppressWarnings(as.integer(x))
  if (is.na(out)) {
    stop("Could not parse integer for ", name, ": ", x)
  }
  out
}

parse_num <- function(x, name) {
  out <- suppressWarnings(as.numeric(x))
  if (is.na(out)) {
    stop("Could not parse numeric for ", name, ": ", x)
  }
  out
}

usage <- function() {
  cat(
    "Usage:\n",
    "  Rscript R/run_adnuts_mcmc.R <model|ctl> [options]\n\n",
    "Examples:\n",
    "  Rscript R/run_adnuts_mcmc.R h1_2.00\n",
    "  Rscript R/run_adnuts_mcmc.R config/h1_2.00.ctl --iter=2000 --chains=6 --cores=6\n",
    "  Rscript R/run_adnuts_mcmc.R h1_2.00 --run-label=short2000 --metric=unit --seed=20260317\n",
    "  Rscript R/run_adnuts_mcmc.R h1_2.00 --prepare-only=true\n\n",
    "Options:\n",
    "  --config=config           Directory containing control files\n",
    "  --input=input             Directory containing data files\n",
    "  --results=results         Directory containing fitted model outputs\n",
    "  --exec=../src/jjm         ADMB executable to use with adnuts\n",
    "  --run-root=mcmc_runs      Root directory for staged bases and MCMC runs\n",
    "  --run-label=<label>       Optional label for the run directory\n",
    "  --iter=2000               Iterations per chain\n",
    "  --warmup=<iter/2>         Warmup iterations per chain\n",
    "  --chains=6                Number of chains\n",
    "  --cores=6                 Number of cores to request\n",
    "  --thin=1                  Thinning rate\n",
    "  --seed=12345              Base seed; one seed per chain is generated from this\n",
    "  --metric=unit             adnuts metric: unit or mle\n",
    "  --adapt-delta=0.9         NUTS adapt_delta\n",
    "  --max-treedepth=10        NUTS max_treedepth\n",
    "  --refresh=50              Console refresh interval passed to ADMB\n",
    "  --mceval=false            Run -mceval on merged chains\n",
    "  --skip-monitor=false      Skip Rhat/ESS calculations\n",
    "  --skip-unbounded=true     Skip saving unbounded samples inside the fit object\n",
    "  --prepare-base=true       Build or refresh a prepared base directory before sampling\n",
    "  --reprepare-base=false    Force rebuilding the prepared base directory\n",
    "  --prepare-mode=full       Base prep mode: full, hbf, quick, or skip\n",
    "  --prepare-only=false      Stage and prepare the base, then stop\n",
    "  --dry-run=false           Print planned paths/settings, then stop\n",
    "  --adflags=''              Extra flags appended after -ind <ctl>\n",
    sep = ""
  )
}

read_ctl_value <- function(ctl_file, key) {
  lines <- readLines(ctl_file, warn = FALSE)
  idx <- which(trimws(lines) == paste0("#", key))
  if (length(idx) == 0) {
    stop("Could not find #", key, " in ", ctl_file)
  }
  for (i in seq(idx[1] + 1L, length(lines))) {
    val <- trimws(lines[[i]])
    if (!nzchar(val)) {
      next
    }
    if (startsWith(val, "#")) {
      break
    }
    return(val)
  }
  stop("Could not read value after #", key, " in ", ctl_file)
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(path)) {
    stop("Could not create directory: ", path)
  }
  invisible(path)
}

copy_required <- function(from, to) {
  if (!file.exists(from)) {
    stop("Required file not found: ", from)
  }
  ok <- file.copy(from, to, overwrite = TRUE)
  if (!isTRUE(ok)) {
    stop("Failed to copy ", from, " to ", to)
  }
  invisible(to)
}

copy_tree <- function(from_dir, to_dir) {
  ensure_dir(to_dir)
  files <- list.files(from_dir, all.files = TRUE, no.. = TRUE, full.names = TRUE)
  if (length(files) == 0) {
    return(invisible(to_dir))
  }
  ok <- file.copy(files, to_dir, recursive = TRUE, overwrite = TRUE)
  if (!all(ok)) {
    stop("Failed to copy staged base into run directory: ", to_dir)
  }
  invisible(to_dir)
}

make_run_label <- function(model, iter, chains, metric, label = NULL) {
  if (!is.null(label) && nzchar(label)) {
    return(label)
  }
  stamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  paste(model, paste0("iter", iter), paste0("chains", chains), metric, stamp, sep = "_")
}

prepare_command_args <- function(mode, ctl_name) {
  if (mode == "skip") {
    return(NULL)
  }
  if (mode == "full") {
    return(c("-nox", "-ind", ctl_name))
  }
  if (mode == "hbf") {
    return(c("-nox", "-ind", ctl_name, "-hbf"))
  }
  if (mode == "quick") {
    return(c("-nox", "-ind", ctl_name, "-maxfn", "0", "-phase", "1000", "-hbf"))
  }
  stop("Unknown prepare mode: ", mode)
}

has_hessian_files <- function(path) {
  files <- list.files(path, pattern = "\\.(hes|bar)$|^admodel\\.(hes|cov)$")
  length(files) > 0
}

with_detect_cores_patch <- function(cores, expr) {
  ns <- asNamespace("parallel")
  original <- get("detectCores", envir = ns)

  # adnuts compares the requested cores against parallel::detectCores(),
  # which can be NA on some systems. Patch it locally during the call.
  unlockBinding("detectCores", ns)
  assign("detectCores", function(all.tests = FALSE, logical = TRUE) as.integer(cores), envir = ns)
  lockBinding("detectCores", ns)

  on.exit({
    unlockBinding("detectCores", ns)
    assign("detectCores", original, envir = ns)
    lockBinding("detectCores", ns)
  }, add = TRUE)

  force(expr)
}

derive_summary <- function(fit, model_name, ctl_file, data_file, run_dir, prep_seconds, wall_seconds,
                           chains, cores, iter, warmup, thin, metric, adapt_delta,
                           max_treedepth, seed, adflags) {
  monitor <- fit$monitor
  if (!is.null(monitor)) {
    monitor_df <- data.frame(parameter = rownames(monitor), monitor, row.names = NULL, check.names = FALSE)
    mon_use <- subset(monitor_df, parameter != "lp__")
    max_rhat <- if (nrow(mon_use) > 0) suppressWarnings(max(mon_use$Rhat, na.rm = TRUE)) else NA_real_
    min_ess <- if (nrow(mon_use) > 0) suppressWarnings(min(mon_use$n_eff, na.rm = TRUE)) else NA_real_
    median_ess <- if (nrow(mon_use) > 0) suppressWarnings(median(mon_use$n_eff, na.rm = TRUE)) else NA_real_
  } else {
    monitor_df <- NULL
    max_rhat <- NA_real_
    min_ess <- NA_real_
    median_ess <- NA_real_
  }

  if (!is.null(fit$sampler_params) && length(fit$sampler_params) > 0) {
    div_chain <- vapply(fit$sampler_params, function(x) sum(x[, "divergent__"], na.rm = TRUE), numeric(1))
    td_chain <- vapply(fit$sampler_params, function(x) sum(x[, "treedepth__"] >= fit$max_treedepth, na.rm = TRUE), numeric(1))
    sampler_df <- data.frame(
      chain = seq_along(div_chain),
      divergences = div_chain,
      max_treedepth_hits = td_chain,
      stringsAsFactors = FALSE
    )
    divergences <- sum(div_chain, na.rm = TRUE)
    max_treedepth_hits <- sum(td_chain, na.rm = TRUE)
  } else {
    sampler_df <- NULL
    divergences <- NA_real_
    max_treedepth_hits <- NA_real_
  }

  samples_dim <- dim(fit$samples)
  n_par <- samples_dim[[3]] - 1L
  post_warmup_per_chain <- samples_dim[[1]] - fit$warmup

  summary_df <- data.frame(
    timestamp = format(Sys.time(), "%Y-%m-%d %H:%M:%S"),
    model = model_name,
    ctl = ctl_file,
    data = data_file,
    run_dir = normalizePath(run_dir, winslash = "/", mustWork = FALSE),
    algorithm = fit$algorithm,
    chains = chains,
    cores = cores,
    iter = iter,
    warmup = warmup,
    thin = thin,
    post_warmup_per_chain = post_warmup_per_chain,
    n_parameters = n_par,
    metric = metric,
    adapt_delta = adapt_delta,
    max_treedepth = max_treedepth,
    seed = seed,
    adflags = adflags %||% "",
    prep_seconds = prep_seconds,
    wall_seconds = wall_seconds,
    chain_seconds_total = sum(fit$time.total),
    chain_seconds_mean = mean(fit$time.total),
    divergences = divergences,
    max_treedepth_hits = max_treedepth_hits,
    max_rhat = max_rhat,
    min_bulk_ess = min_ess,
    median_bulk_ess = median_ess,
    stringsAsFactors = FALSE
  )

  list(summary = summary_df, monitor = monitor_df, sampler = sampler_df)
}

append_run_log <- function(log_file, row_df) {
  if (file.exists(log_file)) {
    old <- read.csv(log_file, stringsAsFactors = FALSE)
    both <- rbind(old, row_df)
  } else {
    both <- row_df
  }
  write.csv(both, log_file, row.names = FALSE)
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0 || any(args %in% c("-h", "--help"))) {
  usage()
  quit(save = "no", status = 0)
}

opts <- list(
  config = "config",
  input = "input",
  results = "results",
  exec = "../src/jjm",
  run_root = "mcmc_runs",
  run_label = NULL,
  iter = 2000L,
  warmup = NULL,
  chains = 6L,
  cores = 6L,
  thin = 1L,
  seed = 12345L,
  metric = "unit",
  adapt_delta = 0.9,
  max_treedepth = 10L,
  refresh = 50L,
  mceval = FALSE,
  skip_monitor = FALSE,
  skip_unbounded = TRUE,
  prepare_base = TRUE,
  reprepare_base = FALSE,
  prepare_mode = "full",
  prepare_only = FALSE,
  dry_run = FALSE,
  adflags = ""
)

target <- NULL

for (arg in args) {
  if (!startsWith(arg, "--")) {
    if (!is.null(target)) {
      stop("Only one model/control target may be supplied")
    }
    target <- arg
    next
  }

  kv <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
  key <- kv[[1]]
  value <- if (length(kv) > 1) paste(kv[-1], collapse = "=") else ""

  if (key == "config") opts$config <- value
  else if (key == "input") opts$input <- value
  else if (key == "results") opts$results <- value
  else if (key == "exec") opts$exec <- value
  else if (key == "run-root") opts$run_root <- value
  else if (key == "run-label") opts$run_label <- value
  else if (key == "iter") opts$iter <- parse_int(value, key)
  else if (key == "warmup") opts$warmup <- parse_int(value, key)
  else if (key == "chains") opts$chains <- parse_int(value, key)
  else if (key == "cores") opts$cores <- parse_int(value, key)
  else if (key == "thin") opts$thin <- parse_int(value, key)
  else if (key == "seed") opts$seed <- parse_int(value, key)
  else if (key == "metric") opts$metric <- value
  else if (key == "adapt-delta") opts$adapt_delta <- parse_num(value, key)
  else if (key == "max-treedepth") opts$max_treedepth <- parse_int(value, key)
  else if (key == "refresh") opts$refresh <- parse_int(value, key)
  else if (key == "mceval") opts$mceval <- parse_bool(value)
  else if (key == "skip-monitor") opts$skip_monitor <- parse_bool(value)
  else if (key == "skip-unbounded") opts$skip_unbounded <- parse_bool(value)
  else if (key == "prepare-base") opts$prepare_base <- parse_bool(value)
  else if (key == "reprepare-base") opts$reprepare_base <- parse_bool(value)
  else if (key == "prepare-mode") opts$prepare_mode <- value
  else if (key == "prepare-only") opts$prepare_only <- parse_bool(value)
  else if (key == "dry-run") opts$dry_run <- parse_bool(value)
  else if (key == "adflags") opts$adflags <- value
  else stop("Unknown option: --", key)
}

if (is.null(target)) {
  stop("A model name or control file path is required")
}

if (is.null(opts$warmup)) {
  opts$warmup <- floor(opts$iter / 2L)
}

if (!(opts$metric %in% c("unit", "mle"))) {
  stop("Only metric=unit or metric=mle are supported by this wrapper")
}

if (!(opts$prepare_mode %in% c("skip", "full", "hbf", "quick"))) {
  stop("prepare-mode must be one of: skip, full, hbf, quick")
}

if (endsWith(target, ".ctl") || file.exists(target)) {
  ctl_file <- target
  model_name <- sub("\\.ctl$", "", basename(target))
} else {
  model_name <- sub("\\.ctl$", "", basename(target))
  ctl_file <- file.path(opts$config, paste0(model_name, ".ctl"))
}

if (!file.exists(ctl_file)) {
  stop("Control file not found: ", ctl_file)
}

data_name <- read_ctl_value(ctl_file, "dataFile")
data_file <- file.path(opts$input, data_name)
if (!file.exists(data_file)) {
  stop("Data file referenced by control file not found: ", data_file)
}

exec_file <- opts$exec
if (!file.exists(exec_file)) {
  stop("Executable not found: ", exec_file)
}

exec_base <- basename(exec_file)
run_root <- opts$run_root
base_dir <- file.path(run_root, model_name, "base")
run_label <- make_run_label(model_name, opts$iter, opts$chains, opts$metric, opts$run_label)
run_dir <- file.path(run_root, model_name, run_label)

seeds <- opts$seed + seq_len(opts$chains) - 1L

cat("Model:", model_name, "\n")
cat("Control file:", ctl_file, "\n")
cat("Data file:", data_file, "\n")
cat("Executable:", exec_file, "\n")
cat("Base dir:", base_dir, "\n")
cat("Run dir:", run_dir, "\n")
cat("Chains / cores:", opts$chains, "/", opts$cores, "\n")
cat("Iter / warmup / thin:", opts$iter, "/", opts$warmup, "/", opts$thin, "\n")
cat("Metric:", opts$metric, "\n")
cat("Prepare mode:", opts$prepare_mode, "\n")

if (opts$dry_run) {
  quit(save = "no", status = 0)
}

base_needs_build <- opts$prepare_base && (!dir.exists(base_dir) || opts$reprepare_base)
prep_seconds <- 0

if (base_needs_build) {
  unlink(base_dir, recursive = TRUE, force = TRUE)
  ensure_dir(base_dir)

  copy_required(exec_file, file.path(base_dir, exec_base))
  copy_required(ctl_file, file.path(base_dir, basename(ctl_file)))
  copy_required(data_file, file.path(base_dir, basename(data_file)))

  par_src <- file.path(opts$results, paste0(model_name, ".par"))
  cor_src <- file.path(opts$results, paste0(model_name, ".cor"))
  rep_src <- file.path(opts$results, paste0(model_name, ".rep"))
  std_src <- file.path(opts$results, paste0(model_name, ".std"))

  if (file.exists(par_src)) {
    copy_required(par_src, file.path(base_dir, paste0(exec_base, ".par")))
  }
  if (file.exists(cor_src)) {
    copy_required(cor_src, file.path(base_dir, paste0(exec_base, ".cor")))
  }
  if (file.exists(rep_src)) {
    copy_required(rep_src, file.path(base_dir, paste0(exec_base, ".rep")))
  }
  if (file.exists(std_src)) {
    copy_required(std_src, file.path(base_dir, paste0(exec_base, ".std")))
  }

  prep_args <- prepare_command_args(opts$prepare_mode, basename(ctl_file))
  if (!is.null(prep_args)) {
    prep_log <- file.path(base_dir, "prepare.log")
    prep_started <- Sys.time()
    oldwd <- getwd()
    on.exit(setwd(oldwd), add = TRUE)
    setwd(base_dir)
    status <- system2(
      command = file.path(".", exec_base),
      args = prep_args,
      stdout = "prepare.log",
      stderr = "prepare.log",
      wait = TRUE
    )
    setwd(oldwd)
    prep_seconds <- as.numeric(difftime(Sys.time(), prep_started, units = "secs"))
    if (!identical(status, 0L)) {
      stop("Base preparation failed; see ", prep_log)
    }
    if (!has_hessian_files(base_dir)) {
      warning("Base preparation finished but no Hessian-side file was detected in ", base_dir)
    }
  }
}

if (!dir.exists(base_dir)) {
  stop("Prepared base directory does not exist: ", base_dir)
}

if (opts$prepare_only) {
  cat("Prepared base directory is ready at ", normalizePath(base_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
  quit(save = "no", status = 0)
}

if (dir.exists(run_dir)) {
  stop("Run directory already exists: ", run_dir)
}

copy_tree(base_dir, run_dir)

admb_args <- paste(c("-ind", basename(ctl_file), opts$adflags), collapse = " ")
control <- list(
  metric = opts$metric,
  adapt_delta = opts$adapt_delta,
  max_treedepth = opts$max_treedepth,
  refresh = opts$refresh
)

settings <- list(
  model = model_name,
  ctl_file = normalizePath(ctl_file, winslash = "/", mustWork = TRUE),
  data_file = normalizePath(data_file, winslash = "/", mustWork = TRUE),
  exec_file = normalizePath(exec_file, winslash = "/", mustWork = TRUE),
  base_dir = normalizePath(base_dir, winslash = "/", mustWork = TRUE),
  run_dir = normalizePath(run_dir, winslash = "/", mustWork = FALSE),
  iter = opts$iter,
  warmup = opts$warmup,
  chains = opts$chains,
  cores = opts$cores,
  thin = opts$thin,
  seeds = seeds,
  metric = opts$metric,
  adapt_delta = opts$adapt_delta,
  max_treedepth = opts$max_treedepth,
  refresh = opts$refresh,
  mceval = opts$mceval,
  skip_monitor = opts$skip_monitor,
  skip_unbounded = opts$skip_unbounded,
  admb_args = admb_args
)

saveRDS(settings, file.path(run_dir, "settings.rds"))

started <- Sys.time()
fit <- with_detect_cores_patch(opts$cores, {
  sample_nuts(
    model = exec_base,
    path = run_dir,
    iter = opts$iter,
    warmup = opts$warmup,
    chains = opts$chains,
    cores = opts$cores,
    seeds = seeds,
    thin = opts$thin,
    mceval = opts$mceval,
    control = control,
    skip_optimization = TRUE,
    verbose = TRUE,
    skip_monitor = opts$skip_monitor,
    skip_unbounded = opts$skip_unbounded,
    admb_args = admb_args
  )
})
wall_seconds <- as.numeric(difftime(Sys.time(), started, units = "secs"))

saveRDS(fit, file.path(run_dir, "fit.rds"))

diag_out <- derive_summary(
  fit = fit,
  model_name = model_name,
  ctl_file = basename(ctl_file),
  data_file = basename(data_file),
  run_dir = run_dir,
  prep_seconds = prep_seconds,
  wall_seconds = wall_seconds,
  chains = opts$chains,
  cores = opts$cores,
  iter = opts$iter,
  warmup = opts$warmup,
  thin = opts$thin,
  metric = opts$metric,
  adapt_delta = opts$adapt_delta,
  max_treedepth = opts$max_treedepth,
  seed = opts$seed,
  adflags = opts$adflags
)

write.csv(diag_out$summary, file.path(run_dir, "summary.csv"), row.names = FALSE)
if (!is.null(diag_out$monitor)) {
  write.csv(diag_out$monitor, file.path(run_dir, "monitor.csv"), row.names = FALSE)
}
if (!is.null(diag_out$sampler)) {
  write.csv(diag_out$sampler, file.path(run_dir, "sampler_diagnostics.csv"), row.names = FALSE)
}

append_run_log(file.path(run_root, "adnuts_run_log.csv"), diag_out$summary)

cat("\nRun complete:\n")
cat("  Run dir: ", normalizePath(run_dir, winslash = "/", mustWork = FALSE), "\n", sep = "")
cat("  Fit RDS: ", file.path(run_dir, "fit.rds"), "\n", sep = "")
cat("  Summary: ", file.path(run_dir, "summary.csv"), "\n", sep = "")
if (!is.null(diag_out$monitor)) {
  cat("  Monitor: ", file.path(run_dir, "monitor.csv"), "\n", sep = "")
}
if (!is.null(diag_out$sampler)) {
  cat("  Sampler diagnostics: ", file.path(run_dir, "sampler_diagnostics.csv"), "\n", sep = "")
}
print(fit)
