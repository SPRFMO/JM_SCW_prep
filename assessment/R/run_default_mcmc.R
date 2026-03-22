#!/usr/bin/env Rscript

get_script_dir <- function() {
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 0) {
    stop("This wrapper must be run with Rscript")
  }
  dirname(normalizePath(sub("^--file=", "", file_arg[[1]]), mustWork = TRUE))
}

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 0) {
  stop("Usage: Rscript assessment/R/run_default_mcmc.R <model|ctl> [extra adnuts wrapper args]")
}

target <- args[[1]]
extra_args <- args[-1]

orig_wd <- getwd()
script_dir <- get_script_dir()
assessment_dir <- normalizePath(file.path(script_dir, ".."), mustWork = TRUE)
runner <- file.path(script_dir, "run_adnuts_mcmc.R")

if (!file.exists(runner)) {
  stop("Could not find generic MCMC runner: ", runner)
}

if ((endsWith(target, ".ctl") || file.exists(target)) && file.exists(target)) {
  target <- normalizePath(target, mustWork = TRUE)
}

default_args <- c(
  runner,
  target,
  "--iter=1000",
  "--warmup=200",
  "--chains=4",
  "--cores=4"
)

oldwd <- getwd()
on.exit(setwd(oldwd), add = TRUE)
setwd(assessment_dir)

cat(
  "Launching default MCMC for ", target, " with defaults:\n",
  "  iter=1000\n",
  "  warmup=500\n",
  "  chains=4\n",
  "  cores=4\n",
  "Extra args: ", if (length(extra_args)) paste(extra_args, collapse = " ") else "<none>", "\n",
  sep = ""
)

status <- system2("Rscript", args = c(default_args, extra_args), stdout = "", stderr = "")
quit(save = "no", status = status)
