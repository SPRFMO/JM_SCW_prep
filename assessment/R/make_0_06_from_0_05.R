#!/usr/bin/env Rscript

# Build the SCW16 0.06 data/control set from 0.05 by applying the agreed
# Chilean South-Central CPUE effort-creep correction.

tag_index <- function(lines, tag) {
  i <- match(tag, trimws(lines))
  if (is.na(i)) {
    stop("Missing tag: ", tag, call. = FALSE)
  }
  i
}

line_tokens <- function(line) {
  strsplit(trimws(line), "[[:space:]]+")[[1]]
}

format_num <- function(x) {
  format(signif(x, 12), scientific = FALSE, trim = TRUE)
}

replace_after <- function(lines, tag, value) {
  i <- tag_index(lines, tag)
  lines[i + 1] <- value
  lines
}

row_after_tag <- function(lines, tag, row) {
  i <- tag_index(lines, tag)
  i + row
}

stop_if_not <- function(ok, message) {
  if (!isTRUE(ok)) {
    stop(message, call. = FALSE)
  }
}

cpue <- read.delim("data/chile_cpue_2025.tsv", check.names = FALSE)
stop_if_not(all(c("Year_num", "mean_cpue", "wtd_cv", "CPUE+creep") %in% names(cpue)),
            "data/chile_cpue_2025.tsv must include Year_num, mean_cpue, wtd_cv, and CPUE+creep")

creep_multiplier <- ifelse(cpue$Year_num <= 2004,
                           1 / 1.01^(cpue$Year_num - 1994),
                           0.669)
expected_cpue_creep <- cpue$mean_cpue * creep_multiplier
stop_if_not(isTRUE(all.equal(cpue[["CPUE+creep"]], expected_cpue_creep, tolerance = 1e-10)),
            "CPUE+creep does not match the agreed effort-creep correction")

dat <- readLines("input/0.05.dat", warn = FALSE)
inames <- strsplit(trimws(dat[tag_index(dat, "#Inames") + 1]), "%", fixed = TRUE)[[1]]
chile_cpue_row <- match("Chile_CPUE", inames)
stop_if_not(!is.na(chile_cpue_row), "Chile_CPUE index not found in input/0.05.dat")

years_line <- row_after_tag(dat, "#Iyears", chile_cpue_row)
index_line <- row_after_tag(dat, "#Index", chile_cpue_row)
indexerr_line <- row_after_tag(dat, "#Indexerr", chile_cpue_row)

input_years <- as.integer(line_tokens(dat[years_line]))
input_index <- as.numeric(line_tokens(dat[index_line]))
input_indexerr <- as.numeric(line_tokens(dat[indexerr_line]))

stop_if_not(identical(input_years, cpue$Year_num),
            "Chile_CPUE years in input/0.05.dat do not match data/chile_cpue_2025.tsv")
stop_if_not(isTRUE(all.equal(input_index, cpue$mean_cpue, tolerance = 1e-8)),
            "Chile_CPUE index in input/0.05.dat does not match mean_cpue")
stop_if_not(isTRUE(all.equal(input_indexerr, cpue$mean_cpue * cpue$wtd_cv, tolerance = 1e-8)),
            "Chile_CPUE index error in input/0.05.dat does not match mean_cpue * wtd_cv")

dat[index_line] <- paste(format_num(cpue[["CPUE+creep"]]), collapse = " ")
dat[indexerr_line] <- paste(format_num(cpue[["CPUE+creep"]] * cpue$wtd_cv), collapse = " ")
writeLines(dat, "input/0.06.dat")

for (hyp in c("h1", "h2")) {
  ctl <- readLines(file.path("config", paste0(hyp, "_0.05.ctl")), warn = FALSE)
  ctl <- replace_after(ctl, "#dataFile", "0.06.dat ")
  ctl <- replace_after(ctl, "#modelName", paste0(hyp, "_0.06 "))
  writeLines(ctl, file.path("config", paste0(hyp, "_0.06.ctl")))
}

cat("Wrote input/0.06.dat, config/h1_0.06.ctl, and config/h2_0.06.ctl\n")
