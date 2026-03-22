# jjm_rtmb.R  --  RTMB port of JJM (Joint Jack Mackerel Model)
#
# Usage:
#   source("rtmb_jjm/read_jjm.R")
#   source("rtmb_jjm/read_par.R")
#   source("rtmb_jjm/jjm_rtmb.R")
#
#   d   <- read_jjm("assessment/config/h1_1.02.ctl")
#   p   <- read_jjm_par("assessment/results/h1_1.02.par", d)
#   rep <- jjm_run(p, d)          # plain-R step-through; returns full named list
#   # Compare to ADMB: rep$Sp_Biom, rep$pred_ind, rep$nll_components, ...
#
#   obj <- make_jjm_obj(d, p)     # RTMB MakeADFun object for optimization
#   obj$fn()                      # evaluate NLL
#   obj$report()                  # all REPORT() quantities
#
# TODOs (not yet implemented, marked inline):
#   - Mage_offset, M_rw (age-varying / time-varying M)
#   - Selectivity types 2 (logistic) and 3 (double-logistic)
#   - Length compositions (fsh and ind)
#   - Projection years (rec_dev_future, future_projections)
#   - get_msy / Calc_Dependent_Vars
#   - Growth / VB weight-at-age (currently uses fixed wt from ctl/dat)

# ============================================================
# CORE MODEL FUNCTION  (plain R; also called by make_jjm_obj)
# ============================================================

jjm_run <- function(p, d) {

  dat <- d$dat
  ctl <- d$ctl

  # ---- dimensions -------------------------------------------------------
  nfsh      <- dat$nfsh
  nind      <- dat$nind
  nages     <- dat$nages
  nyrs      <- dat$nyrs
  styr      <- dat$styr
  endyr     <- dat$endyr
  styr_rec  <- dat$styr_rec   # first rec-dev year (= styr - nages + rec_age)
  nyrs_rec  <- endyr - styr_rec + 1L
  spmo_frac <- dat$spmo_frac

  nstk   <- ctl$nstk
  nregs  <- ctl$nregs
  SrType <- ctl$SrType        # 2 = Beverton-Holt

  yr_vec <- seq.int(styr, endyr)

  # helpers: year → 1-based indices
  yi <- function(y) y - styr     + 1L   # index into nyrs-length arrays
  ri <- function(y) y - styr_rec + 1L   # index into rec_dev [nyrs_rec]

  # ============================================================
  # 1. NATURAL MORTALITY  (Get_NatMortality)
  # ============================================================
  # For h1_1.02: nstk=1, nreg=1, nmort=1, no age-varying M, no rw M
  Mest <- p$Mest

  M_mat <- matrix(Mest[1], nyrs, nages)   # [nyrs × nages]
  # TODO: Mage_offset (age-varying offsets)
  # TODO: M_rw       (time-varying random-walk M)

  surv_styr <- exp(-M_mat[1, ])           # survival using start-year M

  # ============================================================
  # 2. SELECTIVITY  (Get_Selectivity)
  # ============================================================
  # Result: log_sel_fsh / log_sel_ind – lists of [nyrs × nages] matrices
  # Also track avgsel per block for the normalization penalty in Sel_Like.

  avgsel_fsh <- vector("list", nfsh)
  log_sel_fsh <- vector("list", nfsh)

  for (k in seq_len(nfsh)) {
    stype  <- ctl$fsh_sel_opt[k]
    mat    <- matrix(0.0, nyrs, nages)

    if (stype == 1) {
      nsel   <- ctl$nselages_fsh[k]
      nch    <- ctl$n_sel_ch_fsh[k]
      yrs_ch <- ctl$yrs_sel_ch_fsh[[k]]
      scoeff <- p$log_selcoffs_fsh[[k]]   # matrix [nch × nsel]

      avg_k <- numeric(nch)
      for (ic in seq_len(nch))
        avg_k[ic] <- log(mean(exp(scoeff[ic, ])))
      avgsel_fsh[[k]] <- avg_k

      ich <- 1L
      sel_cur <- scoeff[1L, ]             # current block coefficients
      for (yii in seq_len(nyrs)) {
        yr <- yr_vec[yii]
        if (ich < nch && yr == yrs_ch[ich + 1L]) {
          ich     <- ich + 1L
          sel_cur <- scoeff[ich, ]
        }
        lsel <- numeric(nages)
        lsel[seq_len(nsel)]    <- sel_cur
        lsel[nsel:nages]       <- lsel[nsel]   # hold last value for plus ages
        lsel <- lsel - log(mean(exp(lsel)))    # normalize (mean exp = 1)
        mat[yii, ] <- lsel
      }

    } else if (stype == 2) {
      # TODO: single logistic  (logsel_slope_fsh, sel50_fsh)
      avgsel_fsh[[k]] <- rep(0, ctl$n_sel_ch_fsh[k])

    } else if (stype == 3) {
      # TODO: double logistic  (logsel_p1/p2/p3_fsh)
      avgsel_fsh[[k]] <- rep(0, ctl$n_sel_ch_fsh[k])
    }

    log_sel_fsh[[k]] <- mat
  }

  avgsel_ind <- vector("list", nind)
  log_sel_ind <- vector("list", nind)

  for (k in seq_len(nind)) {
    stype  <- ctl$ind_sel_opt[k]
    mat    <- matrix(0.0, nyrs, nages)

    if (stype == 1) {
      nsel   <- ctl$nselages_ind[k]
      nch    <- ctl$n_sel_ch_ind[k]
      yrs_ch <- ctl$yrs_sel_ch_ind[[k]]
      scoeff <- p$log_selcoffs_ind[[k]]   # matrix [nch × nsel]
      amin   <- ctl$q_age_min[k]          # 1-based, already adjusted in read_jjm
      amax   <- ctl$q_age_max[k]

      avg_k <- numeric(nch)
      for (ic in seq_len(nch))
        avg_k[ic] <- log(mean(exp(scoeff[ic, ])))
      avgsel_ind[[k]] <- avg_k

      ich <- 1L
      sel_cur <- scoeff[1L, ]
      for (yii in seq_len(nyrs)) {
        yr <- yr_vec[yii]
        if (ich < nch && yr == yrs_ch[ich + 1L]) {
          ich     <- ich + 1L
          sel_cur <- scoeff[ich, ]
        }
        lsel <- numeric(nages)
        lsel[seq_len(nsel)]  <- sel_cur
        lsel[nsel:nages]     <- lsel[nsel]
        # normalize over q_age_min:q_age_max (ADMB uses this range for indices)
        lsel <- lsel - log(mean(exp(lsel[amin:amax])))
        mat[yii, ] <- lsel
      }

    } else if (stype == 2) {
      # TODO: asymptotic logistic  (logsel_slope_ind, sel50_ind)
      avgsel_ind[[k]] <- rep(0, ctl$n_sel_ch_ind[k])

    } else if (stype == 3) {
      # TODO: double logistic  (logsel_p1_ind, sel_p2_ind, logsel_p3_ind)
      avgsel_ind[[k]] <- rep(0, ctl$n_sel_ch_ind[k])
    }

    log_sel_ind[[k]] <- mat
  }

  # Apply sel_map: some fisheries / indices share selectivity
  # sel_map is ctl$sel_map [3 × (nfsh+nind)]
  #   row 1: stock index
  #   row 2: type (1=fsh, 2=ind) — meaningful for indices only
  #   row 3: which selectivity index to use
  sel_map <- ctl$sel_map

  for (k in seq_len(nfsh)) {
    if (sel_map[3, k] != k)
      log_sel_fsh[[k]] <- log_sel_fsh[[sel_map[3, k]]]
  }
  for (k in seq_len(nind)) {
    col <- nfsh + k
    if (sel_map[2, col] != 2) {
      # borrow fishery selectivity
      log_sel_ind[[k]] <- log_sel_fsh[[sel_map[3, col]]]
    } else if (sel_map[3, col] != k) {
      # borrow another index's selectivity
      log_sel_ind[[k]] <- log_sel_ind[[sel_map[3, col]]]
    }
  }

  sel_fsh <- lapply(log_sel_fsh, exp)
  sel_ind <- lapply(log_sel_ind, exp)

  # ============================================================
  # 3. FISHING MORTALITY  (Get_Mortality – Baranov, Popes=0)
  # ============================================================
  # F(k, yi, j) = exp(fmort[k,yi]) * sel_fsh[[k]][yi, j]
  # Z(yi, j)    = M(yi,j) + sum_k  F(k,yi,j)  [for each stock, simplified to 1]

  Z_mat <- M_mat      # [nyrs × nages]  (one stock for h1_1.02)
  F_arr <- array(0.0, dim = c(nfsh, nyrs, nages))

  for (k in seq_len(nfsh)) {
    for (yii in seq_len(nyrs)) {
      Fky <- exp(p$fmort[k, yii]) * sel_fsh[[k]][yii, ]
      F_arr[k, yii, ] <- Fky
      Z_mat[yii, ]    <- Z_mat[yii, ] + Fky
    }
  }
  S_mat <- exp(-Z_mat)   # survival  [nyrs × nages]

  # ============================================================
  # 4. INITIAL CONDITIONS & BZERO  (Get_Bzero)
  # ============================================================
  Rzero <- exp(p$log_Rzero)   # length nregs (= 1 for h1_1.02)

  # Build natage from styr_rec to styr using unfished equilibrium then rec_devs
  # natagetmp row i corresponds to year (styr_rec + i - 1); row (nyrs_pre+1) = styr
  nyrs_pre <- styr - styr_rec   # = nages - 1

  natagetmp <- matrix(0.0, nyrs_pre + 1L, nages)

  # Row 1 (= styr_rec): unfished equilibrium
  natagetmp[1L, 1L] <- Rzero[1L]
  for (j in 2:nages)
    natagetmp[1L, j] <- natagetmp[1L, j-1L] * surv_styr[j-1L]
  natagetmp[1L, nages] <- natagetmp[1L, nages] / (1.0 - surv_styr[nages])  # plus group

  # Bzero: unfished spawning biomass at styr_rec
  Bzero <- sum(exp(-M_mat[1L, ] * spmo_frac) * natagetmp[1L, ] * ctl$wt_mature[1L, ])

  # Project styr_rec → styr-1, then set styr
  for (i in seq_len(nyrs_pre)) {
    yr_i <- styr_rec + i - 1L
    natagetmp[i, 1L]              <- exp(p$rec_dev[1L, ri(yr_i)] + p$mean_log_rec[1L])
    natagetmp[i+1L, 2L:nages]    <- natagetmp[i, 1L:(nages-1L)] * surv_styr[1L:(nages-1L)]
    natagetmp[i+1L, nages]       <- natagetmp[i+1L, nages] +
                                    natagetmp[i, nages] * surv_styr[nages]
  }
  natagetmp[nyrs_pre+1L, 1L] <- exp(p$rec_dev[1L, ri(styr)] + p$mean_log_rec[1L])

  natage_init <- natagetmp[nyrs_pre + 1L, ]

  # ============================================================
  # 5. NUMBERS AT AGE  (Get_Numbers_at_Age – Baranov branch)
  # ============================================================
  # natage[yii, j]: row yii = year index, col j = age j
  # row nyrs+1 holds endyr+1 (for next-year calcs)
  natage <- matrix(0.0, nyrs + 1L, nages)
  natage[1L, ] <- natage_init

  # pre-assign recruitments for years styr+1 .. endyr
  for (yii in 2L:nyrs)
    natage[yii, 1L] <- exp(p$mean_log_rec[1L] + p$rec_dev[1L, ri(styr + yii - 1L)])

  catage     <- array(0.0, dim = c(nfsh, nyrs, nages))
  pred_catch <- matrix(0.0, nfsh, nyrs)
  Sp_Biom    <- numeric(nyrs)
  totbiom    <- numeric(nyrs)

  for (yii in seq_len(nyrs)) {
    # Spawning biomass (at fractional point spmo_frac into year)
    Sp_Biom[yii] <- sum(natage[yii, ] * exp(-Z_mat[yii, ] * spmo_frac) * ctl$wt_mature[1L, ])

    # Catch at age (Baranov): C(k,j) = F(k,j)/Z(j) * (1-S(j)) * N(j)
    for (k in seq_len(nfsh)) {
      ca <- F_arr[k, yii, ] / Z_mat[yii, ] * (1.0 - S_mat[yii, ]) * natage[yii, ]
      catage[k, yii, ] <- ca
      pred_catch[k, yii] <- sum(ca * dat$wt_fsh[k, yii, ])
    }

    totbiom[yii] <- sum(natage[yii, ] * ctl$wt_pop[1L, ])

    # Project to next year (also fills row nyrs+1 = endyr+1)
    natage[yii+1L, 2L:nages] <- natage[yii, 1L:(nages-1L)] * S_mat[yii, 1L:(nages-1L)]
    natage[yii+1L, nages]    <- natage[yii+1L, nages] + natage[yii, nages] * S_mat[yii, nages]
  }

  # ============================================================
  # 6. SR CURVE PARAMETERS  (computed inside Get_Numbers_at_Age in ADMB)
  # ============================================================
  # BH: R = SSB / (alpha + beta * SSB)
  # alpha = Bzero * (1 - (h-0.2)/(0.8*h)) / Rzero
  # beta  = (5h - 1) / (4h * Rzero)
  h_stk    <- p$steepness[1L]   # for h1_1.02: nrec=1
  phizero  <- Bzero / Rzero[1L]
  alpha_bh <- Bzero * (1.0 - (h_stk - 0.2) / (0.8 * h_stk)) / Rzero[1L]
  beta_bh  <- (5.0 * h_stk - 1.0) / (4.0 * h_stk * Rzero[1L])

  # SR curve prediction at each year
  pred_rec_sr <- Sp_Biom / (alpha_bh + beta_bh * Sp_Biom)

  # ============================================================
  # 7. SURVEY PREDICTIONS  (Get_Survey_Predictions)
  # ============================================================
  q_power_ind <- exp(p$log_q_power_ind)   # length nind

  pred_ind <- vector("list", nind)
  eac_ind  <- vector("list", nind)

  for (k in seq_len(nind)) {
    nyrs_k <- dat$nyrs_ind[k]
    yrs_k  <- dat$yrs_ind[[k]]
    frac_k <- dat$ind_month_frac[k]

    # base catchability (first observation year)
    q_k <- rep(exp(p$log_q_ind[k]), nyrs_k)

    # random-walk q updates
    npars_rw <- ctl$npars_rw_q[k]
    if (npars_rw > 0L) {
      rw_q   <- p$log_rw_q_ind[[k]]
      yrs_rw <- ctl$yrs_rw_q[[k]]
      for (ii in seq_len(npars_rw)) {
        # ADMB: ii_obs = yrs_rw_q(k,ii) - yrs_ind(k,1) + 1
        ii_obs <- yrs_rw[ii] - yrs_k[1L] + 1L
        q_k[ii_obs] <- q_k[ii_obs - 1L] * exp(rw_q[ii])
        if (ii_obs < nyrs_k)
          q_k[(ii_obs + 1L):nyrs_k] <- q_k[ii_obs]
      }
    }

    # predicted index biomass (q * (N * S^frac * sel * wt) ^ q_power)
    pred_k <- numeric(nyrs_k)
    for (i in seq_len(nyrs_k)) {
      yr    <- yrs_k[i]
      yii   <- yi(yr)
      bm_i  <- sum(natage[yii, ] * S_mat[yii, ]^frac_k *
                   sel_ind[[k]][yii, ] * dat$wt_ind[k, yii, ])
      pred_k[i] <- q_k[i] * bm_i^q_power_ind[k]
    }
    pred_ind[[k]] <- pred_k

    # expected age composition
    nyrs_ka <- dat$nyrs_ind_age[k]
    eac_k   <- matrix(0.0, nyrs_ka, nages)
    for (i in seq_len(nyrs_ka)) {
      yr   <- dat$yrs_ind_age[[k]][i]
      yii  <- yi(yr)
      tmp  <- natage[yii, ] * S_mat[yii, ]^frac_k * sel_ind[[k]][yii, ]
      eac_k[i, ] <- tmp / sum(tmp)
    }
    eac_ind[[k]] <- eac_k
  }

  # ============================================================
  # 8. FISHERY AGE COMP PREDICTIONS  (Get_Fishery_Predictions)
  # ============================================================
  eac_fsh <- vector("list", nfsh)
  for (k in seq_len(nfsh)) {
    nyrs_k <- dat$nyrs_fsh_age[k]
    eac_k  <- matrix(0.0, nyrs_k, nages)
    for (i in seq_len(nyrs_k)) {
      yr  <- dat$yrs_fsh_age[[k]][i]
      yii <- yi(yr)
      ca  <- catage[k, yii, ]
      eac_k[i, ] <- ca / sum(ca)
    }
    eac_fsh[[k]] <- eac_k
  }

  # ============================================================
  # 9. LIKELIHOODS
  # ============================================================

  # -- 9a. Catch likelihood (Cat_Like) – lognormal (phase >= 4 form) ----------
  catch_like <- numeric(nfsh)
  for (k in seq_len(nfsh)) {
    for (yii in seq_len(nyrs)) {
      obs  <- dat$catch_bio[k, yii]
      pred <- pred_catch[k, yii]
      lva  <- dat$catch_bio_lva[k, yii]
      catch_like[k] <- catch_like[k] +
        0.5 * (log(obs + 1e-4) - log(pred + 1e-4))^2 / lva
    }
  }

  # -- 9b. Survey index likelihood (Srv_Like) – lognormal --------------------
  ind_like <- numeric(nind)
  for (k in seq_len(nind)) {
    for (i in seq_len(dat$nyrs_ind[k])) {
      obs  <- dat$obs_ind[[k]][i]
      pred <- pred_ind[[k]][i]
      lse  <- dat$obs_lse_ind[[k]][i]
      ind_like[k] <- ind_like[k] + (log(obs) - log(pred))^2 / (2.0 * lse^2)
    }
  }

  # -- 9c. Fishery age comp likelihood (Age_Like) – multinomial ---------------
  age_like_fsh <- numeric(nfsh)
  offset_fsh   <- numeric(nfsh)
  for (k in seq_len(nfsh)) {
    for (i in seq_len(dat$nyrs_fsh_age[k])) {
      n_eff <- dat$n_sample_fsh_age[[k]][i]
      oac   <- dat$oac_fsh[[k]][i, ]
      eac   <- eac_fsh[[k]][i, ]
      age_like_fsh[k] <- age_like_fsh[k] - n_eff * sum((oac + 0.001) * log(eac   + 0.001))
      offset_fsh[k]   <- offset_fsh[k]   - n_eff * sum((oac + 0.001) * log(oac   + 0.001))
    }
  }
  age_like_fsh <- age_like_fsh - offset_fsh

  # -- 9d. Index age comp likelihood – multinomial ---------------------------
  age_like_ind <- numeric(nind)
  offset_ind   <- numeric(nind)
  for (k in seq_len(nind)) {
    for (i in seq_len(dat$nyrs_ind_age[k])) {
      n_eff <- dat$n_sample_ind_age[[k]][i]
      oac   <- dat$oac_ind[[k]][i, ]     # mina(k)=1, so all ages
      eac   <- eac_ind[[k]][i, ]
      age_like_ind[k] <- age_like_ind[k] - n_eff * sum((oac + 0.001) * log(eac + 0.001))
      offset_ind[k]   <- offset_ind[k]   - n_eff * sum((oac + 0.001) * log(oac + 0.001))
    }
  }
  age_like_ind <- age_like_ind - offset_ind

  # -- 9e. Recruitment likelihood (Rec_Like, last-phase form) ----------------
  sigmar   <- exp(p$log_sigmar)    # length nrec
  sigmarsq <- sigmar^2

  yr_est     <- ctl$yr_rec_est[[1L]]              # years for SR curve fit
  yr_est_idx <- yr_est - styr + 1L                # 1-based year indices

  chi      <- log(natage[yr_est_idx, 1L]) - log(pred_rec_sr[yr_est_idx])
  SSQRec   <- sum(chi^2)
  m_sig2   <- SSQRec / length(yr_est)

  rec_like <- (SSQRec + m_sig2 / 2.0) / (2.0 * sigmarsq[1L]) +
              length(yr_est) * p$log_sigmar[1L]

  # rec_dev prior for all years (applied in last-phase form)
  # ADMB also separates "inside SR window" from "outside" but simplified here
  rec_like_dev <- 0.5 * sum(p$rec_dev[1L, ]^2) / sigmarsq[1L] +
                  nyrs_rec * p$log_sigmar[1L]
  # Note: ADMB's Rec_Like is more nuanced (see jjm.tpl ll.2877-2989)
  # TODO: implement full outside-window rec_dev penalization

  # -- 9f. Selectivity penalties (Sel_Like) ----------------------------------
  sel_like_fsh <- numeric(nfsh)
  for (k in seq_len(nfsh)) {
    stype     <- ctl$fsh_sel_opt[k]
    nch       <- ctl$n_sel_ch_fsh[k]
    yrs_ch    <- ctl$yrs_sel_ch_fsh[[k]]
    avg_k     <- avgsel_fsh[[k]]

    if (stype == 1) {
      curv_pen  <- ctl$curv_pen_fsh[k]
      seldec_pen <- ctl$seldec_pen_fsh[k]
      nsel       <- ctl$nselages_fsh[k]
      seldecage  <- 5L   # TODO: read seldecage from dat/ctl (ADMB: init_int seldecage)

      for (i in seq_len(nch)) {
        yr    <- yrs_ch[i]
        yii   <- yi(yr)
        lsel  <- log_sel_fsh[[k]][yii, ]

        # Normalization penalty: pulls avgsel toward 0
        sel_like_fsh[k] <- sel_like_fsh[k] + 20.0 * avg_k[i]^2

        # Curvature penalty
        sel_like_fsh[k] <- sel_like_fsh[k] + curv_pen * sum(diff(diff(lsel))^2)

        # Smoothness penalty between successive blocks
        if (i > 1L) {
          sigma2 <- ctl$sel_sigma_fsh[[k]][i]^2
          if (sigma2 > 0) {
            lsel_prev <- log_sel_fsh[[k]][yi(yrs_ch[i-1L]), ]
            sel_like_fsh[k] <- sel_like_fsh[k] +
              0.5 * sum((lsel_prev - lsel)^2) / sigma2
          }
        }

        # Descent penalty (ages seldecage..nsel)
        for (j in seldecage:nsel) {
          d_j <- lsel[j-1L] - lsel[j]
          if (d_j > 0.0)
            sel_like_fsh[k] <- sel_like_fsh[k] + 0.5 * d_j^2 / seldec_pen
        }
      }
    }
    # TODO: sel_like for stype 2 and 3
  }

  sel_like_ind <- numeric(nind)
  for (k in seq_len(nind)) {
    stype  <- ctl$ind_sel_opt[k]
    nch    <- ctl$n_sel_ch_ind[k]
    yrs_ch <- ctl$yrs_sel_ch_ind[[k]]
    avg_k  <- avgsel_ind[[k]]

    if (stype == 1) {
      curv_pen  <- ctl$curv_pen_ind[k]
      seldec_pen <- ctl$seldec_pen_ind[k]
      nsel       <- ctl$nselages_ind[k]
      seldecage  <- 5L  # TODO: read seldecage

      for (i in seq_len(nch)) {
        yr   <- yrs_ch[i]
        yii  <- yi(yr)
        lsel <- log_sel_ind[[k]][yii, ]

        sel_like_ind[k] <- sel_like_ind[k] + 20.0 * avg_k[i]^2
        sel_like_ind[k] <- sel_like_ind[k] + curv_pen * sum(diff(diff(lsel))^2)

        if (i > 1L) {
          sigma2 <- ctl$sel_sigma_ind[[k]][i]^2
          if (sigma2 > 0) {
            lsel_prev <- log_sel_ind[[k]][yi(yrs_ch[i-1L]), ]
            sel_like_ind[k] <- sel_like_ind[k] +
              0.5 * sum((lsel_prev - lsel)^2) / sigma2
          }
        }

        for (j in seldecage:nsel) {
          d_j <- lsel[j-1L] - lsel[j]
          if (d_j > 0.0)
            sel_like_ind[k] <- sel_like_ind[k] + 0.5 * d_j^2 / seldec_pen
        }
      }
    }
    # TODO: sel_like for stype 2 and 3
  }

  # -- 9g. Priors (Compute_priors) -------------------------------------------
  post_priors_indq <- numeric(nind)
  for (k in seq_len(nind)) {
    if (ctl$phase_q[k] > 0L) {
      q1 <- exp(p$log_q_ind[k])
      post_priors_indq[k] <- post_priors_indq[k] +
        (log(q1 / ctl$qprior[k]))^2 / (2.0 * ctl$cvqprior[k]^2)
    }
    if (ctl$phase_q_power[k] > 0L) {
      post_priors_indq[k] <- post_priors_indq[k] +
        (log(q_power_ind[k] / ctl$q_power_prior[k]))^2 /
        (2.0 * ctl$cvq_power_prior[k]^2)
    }
    if (ctl$npars_rw_q[k] > 0L) {
      rw_q_k  <- p$log_rw_q_ind[[k]]
      sig_rw  <- ctl$sigma_rw_q[[k]]
      post_priors_indq[k] <- post_priors_indq[k] + sum(rw_q_k^2 / (2.0 * sig_rw^2))
    }
  }

  post_prior_M <- 0.0
  for (r in seq_len(ctl$nmort)) {
    if (ctl$phase_M[r] > 0L)
      post_prior_M <- post_prior_M +
        (log(Mest[r] / ctl$natmortprior[r]))^2 / (2.0 * ctl$cvnatmortprior[r]^2)
  }

  post_prior_h <- 0.0
  for (r in seq_len(ctl$nrec)) {
    if (ctl$phase_srec[r] > 0L)
      post_prior_h <- post_prior_h +
        (log(p$steepness[r] / ctl$steepnessprior[r]))^2 /
        (2.0 * ctl$cvsteepnessprior[r]^2)
  }

  post_prior_sigmar <- 0.0
  for (r in seq_len(ctl$nrec)) {
    if (ctl$phase_sigmar[r] > 0L)
      post_prior_sigmar <- post_prior_sigmar +
        (log(sigmar[r] / ctl$sigmarprior[r]))^2 / (2.0 * ctl$cvsigmarprior[r]^2)
  }

  # Rzero penalty (mild pull toward mean_log_rec)
  rzero_pen <- 0.0
  for (r in seq_len(nregs)) {
    if (ctl$phase_Rzero[r] > 0L)
      rzero_pen <- rzero_pen + 0.5 * (p$log_Rzero[r] - p$mean_log_rec[r])^2
  }

  # -- 9h. F-mortality penalty (Fmort_Pen, high-phase form) ------------------
  fpen <- 0.0001 * sum((exp(p$fmort) - 0.2)^2)

  # TODO: length comp likelihoods (length_like_fsh, length_like_ind)
  # TODO: rec_dev_future penalty

  # ============================================================
  # 10. TOTAL NLL
  # ============================================================
  nll_components <- c(
    catch_like   = sum(catch_like),
    age_like_fsh = sum(age_like_fsh),
    age_like_ind = sum(age_like_ind),
    ind_like     = sum(ind_like),
    rec_like     = rec_like,
    rec_like_dev = rec_like_dev,
    sel_like_fsh = sum(sel_like_fsh),
    sel_like_ind = sum(sel_like_ind),
    priors_indq  = sum(post_priors_indq),
    prior_M      = post_prior_M,
    prior_h      = post_prior_h,
    prior_sigmar = post_prior_sigmar,
    rzero_pen    = rzero_pen,
    fpen         = fpen
  )

  nll <- sum(nll_components)

  # ============================================================
  # RETURN all quantities for step-through inspection
  # ============================================================
  list(
    nll            = nll,
    nll_components = nll_components,

    # Population dynamics
    natage     = natage,            # [nyrs+1 × nages]; row = year index; row nyrs+1 = endyr+1
    M_mat      = M_mat,             # [nyrs × nages]
    Z_mat      = Z_mat,             # [nyrs × nages]
    S_mat      = S_mat,             # [nyrs × nages]
    F_arr      = F_arr,             # [nfsh × nyrs × nages]
    catage     = catage,            # [nfsh × nyrs × nages]
    pred_catch = pred_catch,        # [nfsh × nyrs]
    Sp_Biom    = Sp_Biom,           # [nyrs] spawning biomass
    totbiom    = totbiom,           # [nyrs] total biomass

    # Reference points
    Rzero      = Rzero,
    Bzero      = Bzero,
    phizero    = phizero,
    alpha_bh   = alpha_bh,
    beta_bh    = beta_bh,
    pred_rec_sr = pred_rec_sr,      # [nyrs] SR-curve predictions

    # Selectivity
    log_sel_fsh = log_sel_fsh,      # list of [nyrs × nages] matrices
    log_sel_ind = log_sel_ind,
    sel_fsh     = sel_fsh,
    sel_ind     = sel_ind,
    avgsel_fsh  = avgsel_fsh,
    avgsel_ind  = avgsel_ind,

    # Predictions vs observations
    pred_ind    = pred_ind,         # list of vectors (one per index)
    eac_fsh     = eac_fsh,         # list of [nyrs_age × nages] matrices
    eac_ind     = eac_ind,

    # Likelihood components (vectors)
    catch_like   = catch_like,
    ind_like     = ind_like,
    age_like_fsh = age_like_fsh,
    age_like_ind = age_like_ind,
    sel_like_fsh = sel_like_fsh,
    sel_like_ind = sel_like_ind,
    post_priors_indq = post_priors_indq
  )
}


# ============================================================
# RTMB WRAPPER  (for optimization; requires library(RTMB))
# ============================================================
#
# The model function above is plain R.  To build an RTMB AD-tape
# for optimization, wrap it using MakeADFun.
#
# NOTE: RTMB requires all parameters to be flat scalars/vectors/matrices.
# Ragged-list parameters (log_selcoffs_fsh, log_rw_q_ind, etc.) must be
# restructured before calling MakeADFun.  Use flatten_pars() / unflatten_pars()
# for this.  For step-through use jjm_run() directly.

make_jjm_obj <- function(d, p, ...) {
  # Capture data in closure
  d_cap <- d

  fn <- function(pars) {
    rep <- jjm_run(pars, d_cap)
    rep$nll
  }

  # library(RTMB)  # caller must load RTMB
  RTMB::MakeADFun(fn, p, silent = TRUE, ...)
}
