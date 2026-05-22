#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  library(jjmR)
})

args <- commandArgs(trailingOnly = TRUE)

get_arg <- function(flag, default = NULL) {
  hit <- which(args == flag)
  if (!length(hit) || hit == length(args)) {
    return(default)
  }
  args[[hit + 1]]
}

drop_flag_values <- function(x) {
  flags_with_values <- c("--peels", "--exec", "--cores", "--output")
  drop <- integer()
  for (flag in flags_with_values) {
    hit <- which(x == flag)
    drop <- c(drop, hit, hit + 1)
  }
  x[-intersect(drop, seq_along(x))]
}

model_names <- drop_flag_values(args)
model_names <- model_names[!startsWith(model_names, "--")]
if (!length(model_names)) {
  model_names <- c("h1_0.05", "h2_0.05")
}

n_peels <- as.integer(get_arg("--peels", "5"))
exec_path <- get_arg("--exec", file.path("..", "src", "jjms"))
output_dir <- get_arg("--output", "results")
cores <- as.integer(get_arg("--cores", "5"))

if (!dir.exists(output_dir)) {
  dir.create(output_dir, recursive = TRUE)
}

if (!file.exists(exec_path)) {
  stop("JJM executable not found: ", exec_path, call. = FALSE)
}

use_parallel <- isTRUE(cores > 1)
if (use_parallel) {
  suppressPackageStartupMessages({
    library(doParallel)
  })
  doParallel::registerDoParallel(cores)
}

for (model_name in model_names) {
  message("Running ", n_peels, "-peel retrospective for ", model_name)
  model <- readJJM(
    model_name,
    path = "config",
    input = "input",
    output = output_dir
  )
  names(model) <- model_name

  retro(
    model = model,
    n = n_peels,
    output = output_dir,
    exec = exec_path,
    parallel = use_parallel
  )
}

message("Retrospective runs complete.")
