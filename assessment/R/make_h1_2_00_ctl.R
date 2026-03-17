#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(jjmR))

args <- commandArgs(trailingOnly = TRUE)

source_ctl <- if (length(args) >= 1) args[[1]] else "config/h1_1.14.ctl"
source_model <- sub("\\.ctl$", "", basename(source_ctl))
source_input <- if (length(args) >= 2) args[[2]] else "input"
source_output <- if (length(args) >= 3) args[[3]] else "results"
target_dat <- if (length(args) >= 4) args[[4]] else "input/2.00.dat"
target_ctl <- if (length(args) >= 5) args[[5]] else "config/h1_2.00.ctl"

old_mod <- readJJM(
  source_model,
  path = dirname(source_ctl),
  input = source_input,
  output = source_output
)

old_data <- old_mod[[1]]$data
old_ctl <- old_mod[[1]]$control
new_data <- jjmR:::`.readDat`(dat = target_dat, version = "2015MS")

keep_idx <- match(new_data$Inames, old_data$Inames)
if (any(is.na(keep_idx))) {
  stop("Could not match all retained indices to source control file")
}

subset_rw_series <- function(values, counts, keep) {
  out <- vector(mode = mode(values), length = 0)
  start <- 1
  for (i in seq_along(counts)) {
    n <- counts[i]
    end <- start + n - 1
    if (keep[i] && n > 0) {
      out <- c(out, values[start:end])
    }
    start <- end + 1
  }
  out
}

new_ctl <- old_ctl
new_ctl$dataFile <- basename(target_dat)
new_ctl$modelName <- sub("\\.ctl$", "", basename(target_ctl))

# SelMatrix is ordered as fisheries first, then indices.
# For h1_2.00 there is one stock, four fisheries, and four retained indices.
# Chile_AcousN is the only retained fishery-independent survey and keeps survey
# selectivity code 2; the remaining retained indices use fishery-linked
# selectivities with codes 1, 1, and 1 on the second row.
new_ctl$SelMatrix <- rbind(
  rep(1, old_data$Fnum + new_data$Inum),
  c(rep(1, old_data$Fnum), 2, 1, 1, 1),
  c(seq_len(old_data$Fnum), seq_len(new_data$Inum))
)

new_ctl$qMatrix <- old_ctl$qMatrix[, keep_idx, drop = FALSE]
new_ctl$qpowMatrix <- old_ctl$qpowMatrix[, keep_idx, drop = FALSE]
new_ctl$RW_q_phases <- old_ctl$RW_q_phases[keep_idx]
new_ctl$RW_nyrs_q <- old_ctl$RW_nyrs_q[keep_idx]
new_ctl$RW_q_yrs <- subset_rw_series(old_ctl$RW_q_yrs, old_ctl$RW_nyrs_q, seq_along(old_ctl$RW_nyrs_q) %in% keep_idx)
new_ctl$RW_q_sigmas <- subset_rw_series(old_ctl$RW_q_sigmas, old_ctl$RW_nyrs_q, seq_along(old_ctl$RW_nyrs_q) %in% keep_idx)
new_ctl$q_agemin <- old_ctl$q_agemin[keep_idx]
new_ctl$q_agemax <- old_ctl$q_agemax[keep_idx]

suffixes <- c("info", "selchangeYear", "selchange", "selbyage")
for (new_i in seq_along(keep_idx)) {
  old_i <- keep_idx[new_i]
  for (suffix in suffixes) {
    new_ctl[[paste0("I", new_i, "_", suffix)]] <- old_ctl[[paste0("I", old_i, "_", suffix)]]
  }
}

drop_names <- unlist(lapply((length(keep_idx) + 1):old_data$Inum, function(i) paste0("I", i, "_", suffixes)))
drop_names <- drop_names[drop_names %in% names(new_ctl)]
new_ctl[drop_names] <- NULL

out_dir <- dirname(target_ctl)
out_file <- basename(target_ctl)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

jjmR:::`.writeJJM`(object = new_ctl, outFile = out_file, path = out_dir, transpose = FALSE)

cat("Wrote", target_ctl, "\n")
cat("Retained control indices:", paste(new_data$Inames, collapse = ", "), "\n")
