#!/usr/bin/env Rscript

# Build the SCW16 0.05 data/control set from the current 0.04 set by adding
# the split Peru acoustic survey indices as new, independent survey fleets.

tag_index <- function(lines, tag) {
  i <- match(tag, trimws(lines))
  if (is.na(i)) {
    stop("Missing tag: ", tag, call. = FALSE)
  }
  i
}

replace_after <- function(lines, tag, value) {
  i <- tag_index(lines, tag)
  lines[i + 1] <- value
  lines
}

insert_after_tag <- function(lines, tag, values) {
  i <- tag_index(lines, tag)
  append(lines, values, after = i)
}

insert_before_tag <- function(lines, tag, values) {
  i <- tag_index(lines, tag)
  append(lines, values, after = i - 1)
}

block_lines <- function(lines, tag, n) {
  i <- tag_index(lines, tag)
  lines[(i + 1):(i + n)]
}

insert_tokens <- function(line, values, after) {
  x <- strsplit(trimws(line), "[[:space:]]+")[[1]]
  paste(append(x, values, after = after), collapse = " ")
}

append_tokens <- function(line, values) {
  x <- strsplit(trimws(line), "[[:space:]]+")[[1]]
  paste(c(x, values), collapse = " ")
}

append_token_rows <- function(lines, tag, values_by_row) {
  i <- tag_index(lines, tag)
  for (j in seq_along(values_by_row)) {
    lines[i + j] <- append_tokens(lines[i + j], values_by_row[[j]])
  }
  lines
}

insert_sel_columns <- function(lines, values_by_row, after) {
  i <- tag_index(lines, "#SelMatrix")
  for (j in seq_along(values_by_row)) {
    lines[i + j] <- insert_tokens(lines[i + j], values_by_row[[j]], after = after)
  }
  lines
}

add_index_blocks <- function(lines) {
  i <- tag_index(lines, "#Pwtatage")
  add <- c(
    "#I7_info",
    "1 6 -4 1 12.5 0",
    "#I7_selbyage",
    "0.8 0.9 0.93 1 1 1 1 1 1 1 1 1",
    "#I8_info",
    "1 6 -4 1 12.5 0",
    "#I8_selbyage",
    "0.8 0.9 1 1 1 1 1 1 1 1 1 1"
  )
  append(lines, add, after = i - 1)
}

read_git_file <- function(ref) {
  x <- system2("git", c("show", ref), stdout = TRUE, stderr = TRUE)
  status <- attr(x, "status")
  if (!is.null(status) && status != 0) {
    stop("Could not read git object: ", ref, "\n", paste(x, collapse = "\n"), call. = FALSE)
  }
  x
}

token_count <- function(line) {
  length(strsplit(trimws(line), "[[:space:]]+")[[1]])
}

stop_if_not <- function(ok, message) {
  if (!isTRUE(ok)) {
    stop(message, call. = FALSE)
  }
}

seasonal_source <- "a6894df962cd587aebe619ea4559f89eceea9d9f:assessment/input/0.04.dat"
old <- read_git_file(seasonal_source)
dat <- readLines("input/0.04.dat", warn = FALSE)

current_names <- trimws(dat[tag_index(dat, "#Inames") + 1])
stop_if_not(
  identical(current_names, "Chile_AcousCS%Chile_AcousN%Chile_CPUE%DEPM%Peru_CPUE%Offshore_CPUE"),
  "input/0.04.dat does not look like the expected no-Peru-acoustic 0.04 file"
)

new_years <- block_lines(old, "#Iyears", 3)[2:3]
new_months <- block_lines(old, "#Imonths", 3)[2:3]
new_index <- block_lines(old, "#Index", 3)[2:3]
new_indexerr <- block_lines(old, "#Indexerr", 3)[2:3]
new_length_years <- block_lines(old, "#Iyearslength", 2)
new_length_sample <- block_lines(old, "#Ilengthsample", 2)
new_proplength <- block_lines(old, "#Iproplength", 43)

old_iwt_start <- tag_index(old, "#Iwtatage") + 1
old_year_start <- as.integer(trimws(old[tag_index(old, "#years") + 1]))
old_year_end <- as.integer(trimws(old[tag_index(old, "#years") + 2]))
n_years <- old_year_end - old_year_start + 1
new_wtatage <- old[old_iwt_start:(old_iwt_start + n_years - 1)]

dat <- replace_after(dat, "#Inum", "8 ")
dat <- replace_after(
  dat,
  "#Inames",
  "Chile_AcousCS%Chile_AcousN%Chile_CPUE%DEPM%Peru_CPUE%Offshore_CPUE%Peru_Acoust_S1%Peru_Acoust_S2 "
)
dat <- insert_before_tag(dat, "#Iyears", c("41", "35"))
dat <- insert_before_tag(dat, "#Imonths", new_years)
dat <- insert_before_tag(dat, "#Index", new_months)
dat <- insert_before_tag(dat, "#Indexerr", new_index)
dat <- insert_before_tag(dat, "#Inumageyears", new_indexerr)
dat <- insert_before_tag(dat, "#Inumlengthyears", c("0", "0"))
dat <- insert_before_tag(dat, "#Iyearsage", c("27", "16"))
dat <- insert_after_tag(dat, "#Iyearslength", new_length_years)
dat <- insert_after_tag(dat, "#Ilengthsample", new_length_sample)
dat <- insert_after_tag(dat, "#Iproplength", new_proplength)
dat <- insert_before_tag(dat, "#Pspwn", c(new_wtatage, new_wtatage))

i_fleet <- tag_index(dat, "#fleet_names")
dat[i_fleet + 1] <- "ind ind ind ind ind ind ind ind fsh fsh fsh fsh"
dat[i_fleet + 2] <- paste(
  "Chile_AcousCS Chile_AcousN Chile_CPUE DEPM Peru_CPUE Offshore_CPUE",
  "Peru_Acoust_S1 Peru_Acoust_S2 N_Chile SC_Chile_PS FarNorth Offshore_Trawl"
)
dat[i_fleet + 3] <- "1 2 3 4 5 6 7 8 1 2 3 4"

writeLines(dat, "input/0.05.dat")

h1 <- readLines("config/h1_0.04.ctl", warn = FALSE)
stop_if_not(all(vapply(block_lines(h1, "#SelMatrix", 3), token_count, integer(1)) == 10),
            "config/h1_0.04.ctl does not have the expected 10 SelMatrix columns")
h1 <- replace_after(h1, "#dataFile", "0.05.dat ")
h1 <- replace_after(h1, "#modelName", "h1_0.05 ")
h1 <- insert_sel_columns(h1, list(c("1", "1"), c("2", "2"), c("5", "6")), after = 10)
h1 <- append_token_rows(h1, "#qMatrix", list(c("1", "1"), c("12", "12"), c("3", "3")))
h1 <- append_token_rows(h1, "#qpowMatrix", list(c("1", "1"), c("1.2", "1.2"), c("-1", "-1")))
h1 <- append_token_rows(h1, "#RW_q_phases", list(c("-1", "-1")))
h1 <- append_token_rows(h1, "#RW_nyrs_q", list(c("0", "0")))
h1 <- append_token_rows(h1, "#q_agemin", list(c("1", "1")))
h1 <- append_token_rows(h1, "#q_agemax", list(c("6", "6")))
h1 <- add_index_blocks(h1)
writeLines(h1, "config/h1_0.05.ctl")

h2 <- readLines("config/h2_0.04.ctl", warn = FALSE)
stop_if_not(all(vapply(block_lines(h2, "#SelMatrix", 3), token_count, integer(1)) == 10),
            "config/h2_0.04.ctl does not have the expected 10 SelMatrix columns")
h2 <- replace_after(h2, "#dataFile", "0.05.dat ")
h2 <- replace_after(h2, "#modelName", "h2_0.05 ")
h2 <- insert_sel_columns(h2, list(c("2", "2"), c("2", "2"), c("5", "6")), after = 10)
h2 <- append_token_rows(h2, "#qMatrix", list(c("1", "1"), c("12", "12"), c("3", "3")))
h2 <- append_token_rows(h2, "#qpowMatrix", list(c("1", "1"), c("1.2", "1.2"), c("-1", "-1")))
h2 <- append_token_rows(h2, "#RW_q_phases", list(c("-1", "-1")))
h2 <- append_token_rows(h2, "#RW_nyrs_q", list(c("0", "0")))
h2 <- append_token_rows(h2, "#q_agemin", list(c("1", "1")))
h2 <- append_token_rows(h2, "#q_agemax", list(c("6", "6")))
h2 <- add_index_blocks(h2)
writeLines(h2, "config/h2_0.05.ctl")

cat("Wrote input/0.05.dat, config/h1_0.05.ctl, and config/h2_0.05.ctl\n")
