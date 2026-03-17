#!/usr/bin/env Rscript

suppressPackageStartupMessages(library(jjmR))

args <- commandArgs(trailingOnly = TRUE)

infile <- if (length(args) >= 1) args[[1]] else "input/1.14.dat"
outfile <- if (length(args) >= 2) args[[2]] else "input/2.00.dat"
drop_names <- if (length(args) >= 3) {
  trimws(strsplit(args[[3]], ",", fixed = TRUE)[[1]])
} else {
  c("Chile_AcousCS", "DEPM", "Peru_Acoustic")
}

subset_index_field <- function(x, keep) {
  if (is.null(dim(x))) {
    return(x[keep])
  }
  if (length(dim(x)) == 2) {
    return(x[, keep, drop = FALSE])
  }
  if (length(dim(x)) == 3) {
    return(x[, , keep, drop = FALSE])
  }
  stop("Unexpected dimension for index-based field")
}

d <- jjmR:::`.readDat`(dat = infile, version = "2015MS")

keep <- !(d$Inames %in% drop_names)
missing_names <- setdiff(drop_names, d$Inames)
if (length(missing_names) > 0) {
  stop("Requested indices not found: ", paste(missing_names, collapse = ", "))
}

index_fields <- c(
  "Inames", "Inumyears", "Iyears", "Imonths", "Index", "Indexerr",
  "Inumageyears", "Inumlengthyears", "Iyearsage", "Iyearslength",
  "Iagesample", "Ilengthsample", "Ipropage", "Iproplength", "Iwtatage"
)

for (nm in index_fields) {
  d[[nm]] <- subset_index_field(d[[nm]], keep)
}

d$Inum <- length(d$Inames)

# Drop early Chile_AcousN abundance-index observations to match the 2.00 data set.
acous_n <- which(d$Inames == "Chile_AcousN")
if (length(acous_n) == 1) {
  early_rows <- which(!is.na(d$Iyears[, acous_n]) & d$Iyears[, acous_n] < 2000)
  d$Iyears[early_rows, acous_n] <- NA
  d$Index[early_rows, acous_n] <- NA
  d$Indexerr[early_rows, acous_n] <- NA
  d$Inumyears[acous_n] <- sum(!is.na(d$Iyears[, acous_n]))
}

d$fleet_names <- rbind(
  data.frame(
    fleet_type = "ind",
    fleet_name = d$Inames,
    fleet_number = seq_along(d$Inames)
  ),
  data.frame(
    fleet_type = "fsh",
    fleet_name = d$Fnames,
    fleet_number = seq_along(d$Fnames)
  )
)

out_dir <- dirname(outfile)
out_file <- basename(outfile)

if (!dir.exists(out_dir)) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
}

jjmR:::`.writeJJM`(object = d, outFile = out_file, path = out_dir)

cat("Wrote", outfile, "\n")
cat("Retained indices:", paste(d$Inames, collapse = ", "), "\n")
