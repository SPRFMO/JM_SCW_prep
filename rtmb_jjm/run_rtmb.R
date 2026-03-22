#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(RTMB)
})

source("jjm_rtmb.R")

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 2) {
  cat("Usage: Rscript run_rtmb.R <dat> <ctl> [par] [--strict=false]\n")
  quit(save = "no", status = 1)
}

parse_bool <- function(x) {
  x <- tolower(x)
  if (x %in% c("true", "t", "1", "yes", "y")) return(TRUE)
  if (x %in% c("false", "f", "0", "no", "n")) return(FALSE)
  stop("Could not parse logical value: ", x)
}

opts <- list(strict = TRUE)

pos_args <- character()
for (arg in args) {
  if (startsWith(arg, "--strict=")) {
    opts$strict <- parse_bool(sub("--strict=", "", arg))
  } else {
    pos_args <- c(pos_args, arg)
  }
}

if (length(pos_args) < 2) {
  stop("Need <dat> and <ctl> arguments")
}

dat_fn <- pos_args[[1]]
ctl_fn <- pos_args[[2]]
par_fn <- if (length(pos_args) >= 3) pos_args[[3]] else NULL

obj <- build_rtmb(dat_fn, ctl_fn, par_fn = par_fn, strict = opts$strict)

cat("RTMB object built. Current nll =", obj$fn(), "\n")
