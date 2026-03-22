# read_jjm.R  --  parse JJM .ctl + .dat files into a named R list
#
# Usage:
#   d <- read_jjm("assessment/config/h1_1.02.ctl")
#   # d$dat  – observation data (from .dat file)
#   # d$ctl  – model config    (from .ctl file)
#   # d$dims – convenience scalars: styr, endyr, nages, nfsh, nind, nyrs, ...

read_jjm <- function(ctl_file) {

  # ---- helpers: token-stream reader ----------------------------------------
  make_reader <- function(file) {
    lines  <- readLines(file, warn = FALSE)
    tokens <- character(0)
    for (ln in lines) {
      ln <- trimws(ln)
      if (nchar(ln) == 0 || startsWith(ln, "#")) next
      tokens <- c(tokens, strsplit(ln, "\\s+")[[1]])
    }
    tokens <- tokens[nchar(tokens) > 0]
    pos    <- 1L
    list(
      nxt  = function()        { v <- tokens[pos]; pos <<- pos + 1L; v },
      read = function(n, int=FALSE) {
        v <- tokens[pos:(pos+n-1L)]; pos <<- pos + n
        if (int) as.integer(v) else as.numeric(v)
      },
      done = function() pos > length(tokens)
    )
  }

  # ---- Step 1: ctl file header – get dat filename and model name -----------
  cr <- make_reader(ctl_file)
  dat_file   <- cr$nxt()
  model_name <- cr$nxt()

  # resolve dat file relative to ctl file location
  ctl_dir  <- dirname(ctl_file)
  dat_path <- file.path(ctl_dir, "..", "input", dat_file)
  if (!file.exists(dat_path))
    dat_path <- file.path(dirname(ctl_file), dat_file)
  if (!file.exists(dat_path))
    stop("Cannot find dat file: ", dat_file, " (tried ", dat_path, ")")

  # ---- Step 2: read entire .dat file ---------------------------------------
  dr <- make_reader(dat_path)

  dat <- list()
  dat$styr       <- dr$read(1, int=TRUE)
  dat$endyr      <- dr$read(1, int=TRUE)
  dat$rec_age    <- dr$read(1, int=TRUE)
  dat$oldest_age <- dr$read(1, int=TRUE)
  dat$nages      <- dat$oldest_age - dat$rec_age + 1L
  dat$nyrs       <- dat$endyr - dat$styr + 1L
  dat$styr_rec   <- dat$styr - dat$nages + dat$rec_age   # first recruitment year
  dat$styr_sp    <- dat$styr_rec - dat$rec_age - 1L      # first spawning biomass year

  dat$nlength  <- dr$read(1, int=TRUE)
  dat$len_bins <- dr$read(dat$nlength)

  dat$nfsh     <- dr$read(1, int=TRUE)
  dat$fsh_names <- strsplit(dr$nxt(), "%")[[1]]

  # catch biomass and CVs: nfsh × nyrs matrices
  dat$catch_bio    <- matrix(dr$read(dat$nfsh * dat$nyrs), dat$nfsh, dat$nyrs, byrow=TRUE)
  dat$catch_bio_sd <- matrix(dr$read(dat$nfsh * dat$nyrs), dat$nfsh, dat$nyrs, byrow=TRUE)
  # convert CV to log-variance (used in catch likelihood)
  dat$catch_bio_lva <- log(dat$catch_bio_sd^2 + 1)

  # fishery composition years (ragged)
  dat$nyrs_fsh_age    <- dr$read(dat$nfsh, int=TRUE)
  dat$nyrs_fsh_length <- dr$read(dat$nfsh, int=TRUE)

  dat$yrs_fsh_age    <- lapply(dat$nyrs_fsh_age,    function(n) if(n>0) dr$read(n,int=TRUE) else integer(0))
  dat$yrs_fsh_length <- lapply(dat$nyrs_fsh_length, function(n) if(n>0) dr$read(n,int=TRUE) else integer(0))

  dat$n_sample_fsh_age    <- lapply(dat$nyrs_fsh_age,    function(n) if(n>0) dr$read(n) else numeric(0))
  dat$n_sample_fsh_length <- lapply(dat$nyrs_fsh_length, function(n) if(n>0) dr$read(n) else numeric(0))

  # age and length compositions (ragged: list of [nyrs × nages] or [nyrs × nlength] matrices)
  dat$oac_fsh <- lapply(dat$nyrs_fsh_age, function(n) {
    if (n > 0) matrix(dr$read(n * dat$nages), n, dat$nages, byrow=TRUE)
    else       matrix(0, 0, dat$nages)
  })
  # normalize each year's age comp to proportions
  dat$oac_fsh <- lapply(dat$oac_fsh, function(m) {
    if (nrow(m) == 0) return(m)
    t(apply(m, 1, function(r) r / sum(r)))
  })

  dat$olc_fsh <- lapply(dat$nyrs_fsh_length, function(n) {
    if (n > 0) matrix(dr$read(n * dat$nlength), n, dat$nlength, byrow=TRUE)
    else       matrix(0, 0, dat$nlength)
  })

  # fishery weight-at-age: array [nfsh × nyrs × nages]
  # File order: for each fleet (outer), for each year, for each age (inner)
  # → read as [nages, nyrs, nfsh] then permute to [nfsh, nyrs, nages]
  dat$wt_fsh <- aperm(array(dr$read(dat$nfsh * dat$nyrs * dat$nages),
                            dim = c(dat$nages, dat$nyrs, dat$nfsh)), c(3L, 2L, 1L))

  # --- indices ---------------------------------------------------------------
  dat$nind      <- dr$read(1, int=TRUE)
  dat$ind_names <- strsplit(dr$nxt(), "%")[[1]]

  dat$nyrs_ind   <- dr$read(dat$nind, int=TRUE)
  dat$yrs_ind    <- lapply(dat$nyrs_ind, function(n) dr$read(n, int=TRUE))
  dat$mo_ind     <- dr$read(dat$nind)          # survey month (1-12)
  dat$obs_ind    <- lapply(dat$nyrs_ind, function(n) dr$read(n))
  dat$obs_se_ind <- lapply(dat$nyrs_ind, function(n) dr$read(n))
  # convert SE (as fraction of obs) to log-SD
  dat$obs_lse_ind <- mapply(function(se, obs) sqrt(log((se/obs)^2 + 1)),
                            dat$obs_se_ind, dat$obs_ind, SIMPLIFY=FALSE)
  dat$ind_month_frac <- (dat$mo_ind - 1) / 12

  dat$nyrs_ind_age    <- dr$read(dat$nind, int=TRUE)
  dat$nyrs_ind_length <- dr$read(dat$nind, int=TRUE)

  dat$yrs_ind_age    <- lapply(dat$nyrs_ind_age,    function(n) if(n>0) dr$read(n,int=TRUE) else integer(0))
  dat$yrs_ind_length <- lapply(dat$nyrs_ind_length, function(n) if(n>0) dr$read(n,int=TRUE) else integer(0))

  dat$n_sample_ind_age    <- lapply(dat$nyrs_ind_age,    function(n) if(n>0) dr$read(n) else numeric(0))
  dat$n_sample_ind_length <- lapply(dat$nyrs_ind_length, function(n) if(n>0) dr$read(n) else numeric(0))

  dat$oac_ind <- lapply(dat$nyrs_ind_age, function(n) {
    if (n > 0) matrix(dr$read(n * dat$nages), n, dat$nages, byrow=TRUE)
    else       matrix(0, 0, dat$nages)
  })
  dat$oac_ind <- lapply(dat$oac_ind, function(m) {
    if (nrow(m) == 0) return(m)
    t(apply(m, 1, function(r) r / sum(r)))
  })

  dat$olc_ind <- lapply(dat$nyrs_ind_length, function(n) {
    if (n > 0) matrix(dr$read(n * dat$nlength), n, dat$nlength, byrow=TRUE)
    else       matrix(0, 0, dat$nlength)
  })

  # index weight-at-age: array [nind × nyrs × nages]
  # Same file order as wt_fsh: fleet outer, age inner
  dat$wt_ind <- aperm(array(dr$read(dat$nind * dat$nyrs * dat$nages),
                            dim = c(dat$nages, dat$nyrs, dat$nind)), c(3L, 2L, 1L))

  dat$spawnmo   <- dr$read(1)
  dat$spmo_frac <- (dat$spawnmo - 1) / 12

  # age-reading error matrix [nages × nages]
  dat$age_err <- matrix(dr$read(dat$nages^2), dat$nages, dat$nages, byrow=TRUE)

  # ---- Step 3: read the control file (already positioned after header) -----
  ctl <- list()
  ctl$dat_file   <- dat_file
  ctl$model_name <- model_name
  ctl$nstk       <- cr$read(1, int=TRUE)
  ctl$stk_names  <- strsplit(cr$nxt(), "%")[[1]]

  nfsh_and_ind   <- dat$nfsh + dat$nind
  ctl$sel_map    <- matrix(cr$read(3 * nfsh_and_ind, int=TRUE), 3, nfsh_and_ind, byrow=TRUE)
  # Row 1: stock index, row 2: type (1=fsh, 2=ind), row 3: index within type

  ctl$nreg       <- cr$read(ctl$nstk, int=TRUE)  # regimes per stock
  nregs          <- sum(ctl$nreg)
  ctl$nregs      <- nregs

  ctl$SrType     <- cr$read(1, int=TRUE)   # 2=BH, 1=Ricker
  ctl$use_age_err <- cr$read(1, int=TRUE)
  ctl$retro      <- cr$read(1, int=TRUE)

  # rec_map [nstk × max(nreg)]
  ctl$rec_map    <- matrix(cr$read(ctl$nstk * max(ctl$nreg), int=TRUE),
                           ctl$nstk, max(ctl$nreg))
  ctl$nrec       <- max(ctl$rec_map)

  # SR priors (length nrec)
  ctl$steepnessprior   <- cr$read(ctl$nrec)
  ctl$cvsteepnessprior <- cr$read(ctl$nrec)
  ctl$phase_srec       <- cr$read(ctl$nrec, int=TRUE)

  ctl$sigmarprior      <- cr$read(ctl$nrec)
  ctl$cvsigmarprior    <- cr$read(ctl$nrec)
  ctl$phase_sigmar     <- cr$read(ctl$nrec, int=TRUE)

  ctl$phase_Rzero      <- cr$read(nregs, int=TRUE)

  # Recruitment estimation windows (ragged: nregs)
  ctl$nrecs_est_shift <- cr$read(nregs, int=TRUE)
  ctl$yr_rec_est      <- lapply(ctl$nrecs_est_shift, function(n) cr$read(n, int=TRUE))

  # Regime shift years [nstk × (max(nreg)-1)]  – only when nreg > 1
  if (max(ctl$nreg) > 1) {
    ctl$reg_shift <- matrix(cr$read(ctl$nstk * (max(ctl$nreg)-1), int=TRUE),
                            ctl$nstk, max(ctl$nreg)-1)
  } else {
    ctl$reg_shift <- matrix(integer(0), ctl$nstk, 0)
  }

  # Growth map [nstk × max(nreg)]
  ctl$growth_map <- matrix(cr$read(ctl$nstk * max(ctl$nreg), int=TRUE),
                           ctl$nstk, max(ctl$nreg))
  ngrowth <- max(ctl$growth_map)
  ctl$ngrowth <- ngrowth

  # Growth priors (length ngrowth each)
  ctl$Linfprior   <- cr$read(ngrowth); ctl$cvLinfprior <- cr$read(ngrowth); ctl$phase_Linf <- cr$read(ngrowth, int=TRUE)
  ctl$kprior      <- cr$read(ngrowth); ctl$cvkprior    <- cr$read(ngrowth); ctl$phase_k    <- cr$read(ngrowth, int=TRUE)
  ctl$Loprior     <- cr$read(ngrowth); ctl$cvLoprior   <- cr$read(ngrowth); ctl$phase_Lo   <- cr$read(ngrowth, int=TRUE)
  ctl$sdageprior  <- cr$read(ngrowth); ctl$cvsdageprior<- cr$read(ngrowth); ctl$phase_sdage<- cr$read(ngrowth, int=TRUE)

  # Mortality map [nstk × max(nreg)]
  ctl$mort_map <- matrix(cr$read(ctl$nstk * max(ctl$nreg), int=TRUE),
                         ctl$nstk, max(ctl$nreg))
  nmort <- max(ctl$mort_map)
  ctl$nmort <- nmort

  # M priors (length nmort)
  ctl$natmortprior   <- cr$read(nmort); ctl$cvnatmortprior <- cr$read(nmort); ctl$phase_M <- cr$read(nmort, int=TRUE)
  ctl$npars_Mage     <- cr$read(nmort, int=TRUE)
  # age-specific M offsets (ragged)
  ctl$ages_M_changes <- lapply(ctl$npars_Mage, function(n) if(n>0) cr$read(n,int=TRUE) else integer(0))
  ctl$Mage_in        <- lapply(ctl$npars_Mage, function(n) if(n>0) cr$read(n) else numeric(0))
  ctl$phase_Mage     <- cr$read(nmort, int=TRUE)

  # time-varying M random walk (per stock)
  ctl$phase_rw_M   <- cr$read(ctl$nstk, int=TRUE)
  ctl$npars_rw_M   <- cr$read(ctl$nstk, int=TRUE)
  ctl$yrs_rw_M     <- lapply(ctl$npars_rw_M, function(n) if(n>0) cr$read(n,int=TRUE) else integer(0))
  ctl$sigma_rw_M   <- lapply(ctl$npars_rw_M, function(n) if(n>0) cr$read(n) else numeric(0))

  # Catchability priors (length nind)
  ctl$qprior          <- cr$read(dat$nind); ctl$cvqprior       <- cr$read(dat$nind); ctl$phase_q       <- cr$read(dat$nind, int=TRUE)
  ctl$q_power_prior   <- cr$read(dat$nind); ctl$cvq_power_prior<- cr$read(dat$nind); ctl$phase_q_power <- cr$read(dat$nind, int=TRUE)
  # q random walk (ragged)
  ctl$phase_rw_q  <- cr$read(dat$nind, int=TRUE)
  ctl$npars_rw_q  <- cr$read(dat$nind, int=TRUE)
  ctl$yrs_rw_q    <- lapply(ctl$npars_rw_q,  function(n) if(n>0) cr$read(n,int=TRUE) else integer(0))
  ctl$sigma_rw_q  <- lapply(ctl$npars_rw_q,  function(n) if(n>0) cr$read(n) else numeric(0))

  ctl$q_age_min <- cr$read(dat$nind, int=TRUE)
  ctl$q_age_max <- cr$read(dat$nind, int=TRUE)
  # convert from age to 1-based age index
  ctl$q_age_min <- ctl$q_age_min - dat$rec_age + 1L
  ctl$q_age_max <- ctl$q_age_max - dat$rec_age + 1L

  ctl$use_vb_wt_age <- cr$read(1, int=TRUE)
  ctl$nproj_yrs     <- cr$read(1, int=TRUE)

  # ---- Per-fishery selectivity blocks --------------------------------------
  ctl$fsh_sel_opt    <- integer(dat$nfsh)
  ctl$nselages_fsh   <- integer(dat$nfsh)
  ctl$phase_sel_fsh  <- integer(dat$nfsh)
  ctl$curv_pen_fsh   <- numeric(dat$nfsh)
  ctl$seldec_pen_fsh <- numeric(dat$nfsh)
  ctl$n_sel_ch_fsh   <- integer(dat$nfsh)          # total blocks (= nchanges + 1)
  ctl$yrs_sel_ch_fsh <- vector("list", dat$nfsh)   # block start years
  ctl$sel_sigma_fsh  <- vector("list", dat$nfsh)   # change smoothness sigmas
  ctl$sel_init_fsh   <- vector("list", dat$nfsh)   # initial sel-at-age values
  ctl$sel_logslp_fsh <- vector("list", dat$nfsh)   # for logistic: log-slope per block
  ctl$sel_inf_fsh    <- vector("list", dat$nfsh)   # for logistic: sel50 per block
  ctl$sel_p1_fsh     <- vector("list", dat$nfsh)   # for double-logistic
  ctl$sel_p2_fsh     <- vector("list", dat$nfsh)
  ctl$sel_p3_fsh     <- vector("list", dat$nfsh)

  for (k in seq_len(dat$nfsh)) {
    stype <- cr$read(1, int=TRUE)
    ctl$fsh_sel_opt[k] <- stype
    if (stype == 1) {   # age-specific coefficients
      ctl$nselages_fsh[k]   <- cr$read(1, int=TRUE)
      ctl$phase_sel_fsh[k]  <- cr$read(1, int=TRUE)
      ctl$curv_pen_fsh[k]   <- cr$read(1)
      ctl$seldec_pen_fsh[k] <- cr$read(1)
      nch <- cr$read(1, int=TRUE) + 1L   # n_sel_ch = nchanges + 1
      ctl$n_sel_ch_fsh[k]   <- nch
      yrs <- integer(nch); yrs[1] <- dat$styr
      if (nch > 1) yrs[2:nch] <- cr$read(nch-1, int=TRUE)
      ctl$yrs_sel_ch_fsh[[k]] <- yrs
      sigs <- numeric(nch)
      if (nch > 1) sigs[2:nch] <- cr$read(nch-1)
      ctl$sel_sigma_fsh[[k]] <- sigs
      ctl$sel_init_fsh[[k]]  <- cr$read(dat$nages)   # initial sel-at-age
    } else if (stype == 2) {  # single logistic
      ctl$phase_sel_fsh[k] <- cr$read(1, int=TRUE)
      nch <- cr$read(1, int=TRUE) + 1L
      ctl$n_sel_ch_fsh[k]  <- nch
      yrs <- integer(nch); yrs[1] <- dat$styr
      if (nch > 1) yrs[2:nch] <- cr$read(nch-1, int=TRUE)
      ctl$yrs_sel_ch_fsh[[k]] <- yrs
      sigs <- numeric(nch)
      if (nch > 1) sigs[2:nch] <- cr$read(nch-1)
      ctl$sel_sigma_fsh[[k]] <- sigs
      ctl$sel_logslp_fsh[[k]] <- log(cr$read(1))   # initial slope
      ctl$sel_inf_fsh[[k]]    <- cr$read(1)         # initial sel50
      ctl$nselages_fsh[k]     <- dat$nages - 1L
    } else if (stype == 3) {  # double logistic
      ctl$nselages_fsh[k]  <- cr$read(1, int=TRUE)
      ctl$phase_sel_fsh[k] <- cr$read(1, int=TRUE)
      nch <- cr$read(1, int=TRUE) + 1L
      ctl$n_sel_ch_fsh[k]  <- nch
      yrs <- integer(nch); yrs[1] <- dat$styr
      if (nch > 1) yrs[2:nch] <- cr$read(nch-1, int=TRUE)
      ctl$yrs_sel_ch_fsh[[k]] <- yrs
      sigs <- numeric(nch)
      if (nch > 1) sigs[2:nch] <- cr$read(nch-1)
      ctl$sel_sigma_fsh[[k]] <- sigs
      ctl$sel_p1_fsh[[k]] <- cr$read(1)
      ctl$sel_p2_fsh[[k]] <- cr$read(1)
      ctl$sel_p3_fsh[[k]] <- cr$read(1)
    }
  }

  # ---- Per-index selectivity blocks ----------------------------------------
  ctl$ind_sel_opt    <- integer(dat$nind)
  ctl$nselages_ind   <- integer(dat$nind)
  ctl$phase_sel_ind  <- integer(dat$nind)
  ctl$curv_pen_ind   <- numeric(dat$nind)
  ctl$seldec_pen_ind <- numeric(dat$nind)
  ctl$n_sel_ch_ind   <- integer(dat$nind)
  ctl$yrs_sel_ch_ind <- vector("list", dat$nind)
  ctl$sel_sigma_ind  <- vector("list", dat$nind)
  ctl$sel_init_ind   <- vector("list", dat$nind)
  ctl$sel_logslp_ind <- vector("list", dat$nind)
  ctl$sel_inf_ind    <- vector("list", dat$nind)
  ctl$sel_p1_ind     <- vector("list", dat$nind)
  ctl$sel_p2_ind     <- vector("list", dat$nind)
  ctl$sel_p3_ind     <- vector("list", dat$nind)

  for (k in seq_len(dat$nind)) {
    stype <- cr$read(1, int=TRUE)
    ctl$ind_sel_opt[k] <- stype
    if (stype == 1) {
      ctl$nselages_ind[k]   <- cr$read(1, int=TRUE)
      ctl$phase_sel_ind[k]  <- cr$read(1, int=TRUE)
      ctl$curv_pen_ind[k]   <- cr$read(1)
      ctl$seldec_pen_ind[k] <- cr$read(1)
      nch <- cr$read(1, int=TRUE) + 1L
      ctl$n_sel_ch_ind[k]   <- nch
      yrs <- integer(nch); yrs[1] <- dat$styr
      if (nch > 1) yrs[2:nch] <- cr$read(nch-1, int=TRUE)
      ctl$yrs_sel_ch_ind[[k]] <- yrs
      sigs <- numeric(nch)
      if (nch > 1) sigs[2:nch] <- cr$read(nch-1)
      ctl$sel_sigma_ind[[k]] <- sigs
      ctl$sel_init_ind[[k]]  <- cr$read(dat$nages)
    } else if (stype == 2) {
      ctl$phase_sel_ind[k] <- cr$read(1, int=TRUE)
      nch <- cr$read(1, int=TRUE) + 1L
      ctl$n_sel_ch_ind[k]  <- nch
      yrs <- integer(nch); yrs[1] <- dat$styr
      if (nch > 1) yrs[2:nch] <- cr$read(nch-1, int=TRUE)
      ctl$yrs_sel_ch_ind[[k]] <- yrs
      sigs <- numeric(nch)
      if (nch > 1) sigs[2:nch] <- cr$read(nch-1)
      ctl$sel_sigma_ind[[k]] <- sigs
      ctl$sel_logslp_ind[[k]] <- log(cr$read(1))
      ctl$sel_inf_ind[[k]]    <- cr$read(1)
      ctl$nselages_ind[k]     <- dat$nages - 1L
    } else if (stype == 3) {
      ctl$nselages_ind[k]  <- cr$read(1, int=TRUE)
      ctl$phase_sel_ind[k] <- cr$read(1, int=TRUE)
      nch <- cr$read(1, int=TRUE) + 1L
      ctl$n_sel_ch_ind[k]  <- nch
      yrs <- integer(nch); yrs[1] <- dat$styr
      if (nch > 1) yrs[2:nch] <- cr$read(nch-1, int=TRUE)
      ctl$yrs_sel_ch_ind[[k]] <- yrs
      sigs <- numeric(nch)
      if (nch > 1) sigs[2:nch] <- cr$read(nch-1)
      ctl$sel_sigma_ind[[k]] <- sigs
      ctl$sel_p1_ind[[k]] <- cr$read(1)
      ctl$sel_p2_ind[[k]] <- cr$read(1)
      ctl$sel_p3_ind[[k]] <- cr$read(1)
    }
  }

  # Population weight-at-age and maturity [nstk × nages]
  ctl$wt_pop    <- matrix(cr$read(ctl$nstk * dat$nages), ctl$nstk, dat$nages, byrow=TRUE)
  ctl$maturity  <- matrix(cr$read(ctl$nstk * dat$nages), ctl$nstk, dat$nages, byrow=TRUE)
  ctl$wt_mature <- ctl$wt_pop * ctl$maturity   # pre-multiply for spawning biomass

  # sentinel check
  test <- cr$read(1)
  if (test != 123456789)
    warning("Control file sentinel mismatch (got ", test, "); file may be misread")

  # ---- convenience dims bundle ---------------------------------------------
  dims <- c(
    dat[c("styr","endyr","rec_age","oldest_age","nages","nyrs","styr_rec","styr_sp",
          "nlength","nfsh","nind","spawnmo")],
    ctl[c("nstk","nreg","nregs","nrec","ngrowth","nmort","nproj_yrs")]
  )

  list(dat = dat, ctl = ctl, dims = dims)
}
