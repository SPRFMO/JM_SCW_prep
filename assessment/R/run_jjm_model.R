#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(jjmR))

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

read_par_summary <- function(repfile_base) {
  parfile <- paste0(repfile_base, ".par")
  vals <- as.numeric(scan(parfile, what = "", n = 16, quiet = TRUE)[c(6, 11, 16)])
  list(nopar = as.integer(vals[1]), nlogl = vals[2], maxgrad = vals[3])
}

usage <- function() {
  cat(
    "Usage:\n",
    "  Rscript R/run_jjm_model.R <model> [<model> ...] [--exec=../src/jjm] [--path=config]\n",
    "      [--input=input] [--output=results] [--pdf=false] [--portrait=false]\n",
    "      [--parallel=false] [--iprint=100] [--est=true] [--adflags='...']\n\n",
    "Examples:\n",
    "  Rscript R/run_jjm_model.R h1_2.00\n",
    "  Rscript R/run_jjm_model.R h1_2.00 h2_2.00 --parallel=true\n",
    "  Rscript R/run_jjm_model.R h1_2.00 --adflags='-tac 1428 -fut_sel 3'\n",
    sep = ""
  )
}

args <- commandArgs(trailingOnly = TRUE)

if (length(args) == 0 || any(args %in% c("-h", "--help"))) {
  usage()
  quit(save = "no", status = 0)
}

opts <- list(
  exec = "../src/jjm",
  path = "config",
  input = "input",
  output = "results",
  pdf = FALSE,
  portrait = FALSE,
  parallel = FALSE,
  iprint = 100,
  est = TRUE,
  adflags = NULL
)

models <- character()

for (arg in args) {
  if (!startsWith(arg, "--")) {
    models <- c(models, sub("\\.ctl$", "", basename(arg)))
    next
  }

  kv <- strsplit(sub("^--", "", arg), "=", fixed = TRUE)[[1]]
  key <- kv[[1]]
  value <- if (length(kv) > 1) paste(kv[-1], collapse = "=") else ""

  if (key == "exec") opts$exec <- value
  else if (key == "path") opts$path <- value
  else if (key == "input") opts$input <- value
  else if (key == "output") opts$output <- value
  else if (key == "pdf") opts$pdf <- parse_bool(value)
  else if (key == "portrait") opts$portrait <- parse_bool(value)
  else if (key == "parallel") opts$parallel <- parse_bool(value)
  else if (key == "iprint") opts$iprint <- as.integer(value)
  else if (key == "est") opts$est <- parse_bool(value)
  else if (key == "adflags") opts$adflags <- value
  else stop("Unknown option: --", key)
}

if (length(models) == 0) {
  stop("At least one model name is required")
}

cat("Running model(s):", paste(models, collapse = ", "), "\n")
cat("Executable:", opts$exec, "\n")
cat("Config path:", opts$path, "\n")
cat("Input path:", opts$input, "\n")
cat("Output path:", opts$output, "\n")

run_args <- list(
  mod = models,
  est = opts$est,
  exec = opts$exec,
  path = opts$path,
  input = opts$input,
  output = opts$output,
  pdf = opts$pdf,
  portrait = opts$portrait,
  parallel = opts$parallel,
  iprint = opts$iprint
)

if (!is.null(opts$adflags) && nzchar(opts$adflags)) {
  run_args$adflags <- opts$adflags
}

res <- do.call(runit, run_args)

cat("\nCompleted models:\n")
for (i in seq_along(models)) {
  mod <- res[[i]][[1]]
  fit <- tryCatch(read_par_summary(file.path(opts$output, models[[i]])), error = function(e) NULL)
  cat("  ", models[[i]], "\n", sep = "")
  cat("    data file: ", mod$data$Inum, " indices, ", mod$data$Fnum, " fisheries\n", sep = "")
  cat("    indices: ", paste(mod$data$Inames, collapse = ", "), "\n", sep = "")
  if (!is.null(fit)) {
    cat("    nll: ", fit$nlogl, "\n", sep = "")
    cat("    maxgrad: ", fit$maxgrad, "\n", sep = "")
  }
}
