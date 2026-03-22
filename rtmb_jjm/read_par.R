# read_par.R  --  load ADMB .par file into a structured parameter list
#
# Usage:
#   d   <- read_jjm("assessment/config/h1_1.02.ctl")
#   par <- read_jjm_par("assessment/results/h1_1.02.par", d)
#
# Returns a named list with the same structure used by jjm_fn() in jjm_rtmb.R.
# Ragged selectivity blocks are stored as lists of matrices.
#
# Note: ALL parameters (including fixed/negative-phase ones) appear in the .par
# file. This reader loads them all, which is correct for step-through comparison.

read_jjm_par <- function(par_file, d) {
  dat <- d$dat; ctl <- d$ctl

  # ---- token reader -------------------------------------------------------
  lines  <- readLines(par_file, warn=FALSE)
  tokens <- character(0)
  labels <- character(0)
  for (ln in lines) {
    ln <- trimws(ln)
    if (nchar(ln) == 0) next
    if (startsWith(ln, "#")) { labels <- c(labels, ln); next }
    tokens <- c(tokens, strsplit(ln, "\\s+")[[1]])
  }
  tokens <- tokens[nchar(tokens) > 0]
  pos    <- 1L
  read_n <- function(n) {
    v <- as.numeric(tokens[pos:(pos+n-1L)]); pos <<- pos + n; v
  }
  read1  <- function() read_n(1)[1]

  # shortcuts
  nstk   <- ctl$nstk;    nreg   <- ctl$nreg;    nregs  <- ctl$nregs
  nrec   <- ctl$nrec;    ngrowth<- ctl$ngrowth;  nmort  <- ctl$nmort
  nfsh   <- dat$nfsh;    nind   <- dat$nind
  nages  <- dat$nages;   nyrs   <- dat$nyrs
  styr   <- dat$styr;    endyr  <- dat$endyr
  styr_rec <- dat$styr_rec
  nyrs_rec <- endyr - styr_rec + 1L
  nproj    <- ctl$nproj_yrs

  p <- list()

  # ---- Natural mortality ---------------------------------------------------
  # init_bounded_number_vector Mest(1,nmort, .02, 4.8, phase_M)
  p$Mest <- read_n(nmort)

  # init_bounded_vector_vector Mage_offset(1,nmort, 1,npars_Mage, -3,3, phase_Mage)
  # Only present in par when any phase_Mage > 0, but ADMB writes zeros if fixed
  p$Mage_offset <- lapply(ctl$npars_Mage, function(n) if(n>0) read_n(n) else numeric(0))

  # init_bounded_vector_vector M_rw(1,nstk, 1,npars_rw_M, -10,10, phase_rw_M)
  p$M_rw <- lapply(ctl$npars_rw_M, function(n) if(n>0) read_n(n) else numeric(0))

  # ---- Growth -------------------------------------------------------------
  p$log_Linf  <- read_n(ngrowth)
  p$log_k     <- read_n(ngrowth)
  p$log_Lo    <- read_n(ngrowth)
  p$log_sdage <- read_n(ngrowth)

  # ---- Stock-Recruit ------------------------------------------------------
  p$mean_log_rec <- read_n(nregs)
  p$steepness    <- read_n(nrec)
  p$log_Rzero    <- read_n(nregs)

  # rec_dev [nstk × nyrs_rec], rows = stocks, cols = styr_rec..endyr
  p$rec_dev <- matrix(read_n(nstk * nyrs_rec), nrow=nstk, ncol=nyrs_rec)

  p$log_sigmar <- read_n(nrec)

  # ---- Fishing mortality --------------------------------------------------
  # init_bounded_matrix fmort(1,nfsh, styr,endyr, -15,15, phase_fmort)
  # Stored in par as nfsh rows, each of length nyrs
  p$fmort <- matrix(read_n(nfsh * nyrs), nrow=nfsh, ncol=nyrs, byrow=TRUE)

  # ---- Fishery selectivity coefficients -----------------------------------
  # init_matrix_vector log_selcoffs_fsh(1,nfsh, 1,n_sel_ch_fsh, 1,nselages_fsh, phase)
  # For each fishery: [n_sel_ch × nselages] matrix
  p$log_selcoffs_fsh <- lapply(seq_len(nfsh), function(k) {
    if (ctl$fsh_sel_opt[k] == 1) {
      nch  <- ctl$n_sel_ch_fsh[k]
      nsel <- ctl$nselages_fsh[k]
      matrix(read_n(nch * nsel), nrow=nch, ncol=nsel, byrow=TRUE)
    } else {
      matrix(0, 0, 0)  # not used for logistic/double-logistic
    }
  })

  # (spline selectivity log_sel_spl_fsh – always 4 nodes, always in par)
  # Read but ignore for now; 4 values per block per fishery
  for (k in seq_len(nfsh)) {
    nch <- ctl$n_sel_ch_fsh[k]
    read_n(nch * 4)  # consume; TODO: store if spline selectivity is needed
  }

  # init_vector_vector logsel_slope_fsh + sel50_fsh (logistic option 2)
  p$logsel_slope_fsh <- lapply(seq_len(nfsh), function(k) {
    nch <- ctl$n_sel_ch_fsh[k]
    if (ctl$fsh_sel_opt[k] == 2) read_n(nch) else { read_n(nch); numeric(0) }
  })
  p$sel50_fsh <- lapply(seq_len(nfsh), function(k) {
    nch <- ctl$n_sel_ch_fsh[k]
    if (ctl$fsh_sel_opt[k] == 2) read_n(nch) else { read_n(nch); numeric(0) }
  })

  # init_vector_vector logsel_p1/p2 + logsel_p3 (double logistic option 3)
  p$logsel_p1_fsh <- lapply(seq_len(nfsh), function(k) {
    nch <- ctl$n_sel_ch_fsh[k]
    if (ctl$fsh_sel_opt[k] == 3) read_n(nch) else { read_n(nch); numeric(0) }
  })
  p$logsel_p2_fsh <- lapply(seq_len(nfsh), function(k) {
    nch <- ctl$n_sel_ch_fsh[k]
    if (ctl$fsh_sel_opt[k] == 3) read_n(nch) else { read_n(nch); numeric(0) }
  })
  p$logsel_p3_fsh <- lapply(seq_len(nfsh), function(k) {
    nch <- ctl$n_sel_ch_fsh[k]
    if (ctl$fsh_sel_opt[k] == 3) read_n(nch) else { read_n(nch); numeric(0) }
  })

  # ---- Future recruitment deviations (phase_proj = 5 when nproj_yrs > 0) --
  p$rec_dev_future <- if (nproj > 0)
    matrix(read_n(nstk * nproj), nrow=nstk, ncol=nproj, byrow=TRUE)
  else
    matrix(0, nstk, 0)

  # ---- Catchability --------------------------------------------------------
  p$log_q_ind       <- read_n(nind)
  p$log_q_power_ind <- read_n(nind)

  # init_vector_vector log_rw_q_ind(1,nind, 1,npars_rw_q, phase_rw_q)
  p$log_rw_q_ind <- lapply(ctl$npars_rw_q, function(n) if(n>0) read_n(n) else numeric(0))

  # ---- Index selectivity coefficients -------------------------------------
  p$log_selcoffs_ind <- lapply(seq_len(nind), function(k) {
    if (ctl$ind_sel_opt[k] == 1) {
      nch  <- ctl$n_sel_ch_ind[k]
      nsel <- ctl$nselages_ind[k]
      matrix(read_n(nch * nsel), nrow=nch, ncol=nsel, byrow=TRUE)
    } else {
      matrix(0, 0, 0)
    }
  })

  # logsel_slope_ind + sel50_ind (logistic option 2)
  p$logsel_slope_ind <- lapply(seq_len(nind), function(k) {
    nch <- ctl$n_sel_ch_ind[k]
    if (ctl$ind_sel_opt[k] == 2) read_n(nch) else { read_n(nch); numeric(0) }
  })
  p$sel50_ind <- lapply(seq_len(nind), function(k) {
    nch <- ctl$n_sel_ch_ind[k]
    if (ctl$ind_sel_opt[k] == 2) read_n(nch) else { read_n(nch); numeric(0) }
  })

  # double-logistic for indices (option 3)
  p$logsel_p1_ind <- lapply(seq_len(nind), function(k) {
    nch <- ctl$n_sel_ch_ind[k]
    if (ctl$ind_sel_opt[k] == 3) read_n(nch) else { read_n(nch); numeric(0) }
  })
  p$sel_p2_ind <- lapply(seq_len(nind), function(k) {
    nch <- ctl$n_sel_ch_ind[k]
    if (ctl$ind_sel_opt[k] == 3) read_n(nch) else { read_n(nch); numeric(0) }
  })
  p$logsel_p3_ind <- lapply(seq_len(nind), function(k) {
    nch <- ctl$n_sel_ch_ind[k]
    if (ctl$ind_sel_opt[k] == 3) read_n(nch) else { read_n(nch); numeric(0) }
  })

  p
}
