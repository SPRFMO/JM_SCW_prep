require(dplyr)
require(ggplot2)

.get_array <- function(mmse, .name,
                       type = c("SB", "B_BMSY", "F_FMSY", "CBA", "Catch_Chile", "Catch_Peru", "Acoustic_Chile", "MPH",
                                "rho", "CBA_ratio", "R"),
                       annual = FALSE) {

  type <- match.arg(type)
  p <- mp <- 1

  Year <- 2023 +seq(1, mmse@proyears)
  Year_hist <- 2023 - (mmse@nyears-1):0

  x <- switch(
    type,
    "SB" = mmse@SSB[, p, , ],
    "B_BMSY" = 2 * mmse@SSB[, p, , ]/mmse@RefPoint$ByYear$SSB0[, p, mp, mmse@nyears],
    "F_FMSY" = apply(mmse@FM[, p, , , ], c(1, 3:4), sum)/mmse@RefPoint$ByYear$F_SPR[, "F_55%", mmse@nyears],
    "CBA" = mmse@Catch[, p, 1, , ], # Chilean catch (f = 1) is equal to CBA
    "Catch_Chile" = mmse@Catch[, p, 1, , ],
    "Catch_Peru" = mmse@Catch[, p, 2, , ],
    "Acoustic_Chile" = NULL,
    "MPH" = NULL,
    "R" = apply(mmse@N[, p, 1, , , ], 1:3, sum),
    NULL
  )

  if (!is.null(x)) {
    x <- x %>%
      structure(dimnames = list(Simulation = 1:mmse@nsim, MP = mmse@MPs[[1]], Year = Year)) %>%
      reshape2::melt()
  }

  nf <- 2
  xhist <- switch(
    type,
    "SB" = apply(mmse@multiHist[[p]][[1]]@TSdata$SBiomass, 1:2, sum),
    "B_BMSY" = 2 * apply(mmse@multiHist[[p]][[1]]@TSdata$SBiomass, 1:2, sum)/mmse@RefPoint$ByYear$SSB0[, 1, 1, mmse@nyears],
    "F_FMSY" = lapply(1:nf, function(f) mmse@multiHist[[1]][[f]]@AtAge$F.Mortality %>% apply(c(1, 3), max)) %>%
      simplify2array() %>% apply(1:2, sum) %>% `/`(mmse@RefPoint$ByYear$F_SPR[, "F_55%", mmse@nyears]),
    "Catch_Chile" = mmse@multiHist[[1]][[1]]@Data@Cat,
    "Catch_Peru" = mmse@multiHist[[1]][[2]]@Data@Cat,
    "R" = apply(mmse@multiHist[[1]][[1]]@AtAge$Number[, 1, , ], 1:2, sum),
    NULL
  )

  if (!is.null(xhist)) {
    xhist <- xhist %>%
      array(c(mmse@nsim, mmse@nyears, mmse@nMPs)) %>%
      aperm(c(1, 3, 2)) %>%
      structure(dimnames = list(Simulation = 1:mmse@nsim, MP = mmse@MPs[[1]], Year = Year_hist)) %>%
      reshape2::melt()
  }

  if (type %in% c("Acoustic_Chile", "MPH")) {

    index_text <- switch(
      type,
      "MPH" = "mmse@PPD[[1]][[1]][[i]]@SpInd",
      "Acoustic_Chile" = "mmse@PPD[[1]][[1]][[i]]@AddInd[, 1, ]"
    )

    index_list <- lapply(1:mmse@nMPs, function(i) {
      ii <- parse(text = index_text) %>% eval()

      Year_out <- c(Year_hist, Year)[1:ncol(ii)]

      ii %>%
        structure(dimnames = list(Simulation = 1:mmse@nsim, Year = Year_out)) %>%
        reshape2::melt() %>%
        mutate(MP = mmse@MPs[[1]][i])
    })

    x <- do.call(rbind, index_list) %>%
      dplyr::filter(!is.na(value))
  }

  if (type == "rho") {
    x <- lapply(1:mmse@nMPs, function(i) {
      lapply(1:mmse@nsim, function(xx) {
        rho <- mmse@PPD[[1]][[1]][[i]]@Misc[[xx]][["rho"]][2, ]

        if (!is.null(rho)) {
          data.frame(Simulation = xx, Year = Year[1:length(rho)], value = rho)
        } else {
          data.frame()
        }
      }) %>%
        dplyr::bind_rows() %>%
        mutate(MP = mmse@MPs[[1]][i])
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(!is.na(value))
  }

  if (type == "CBA_ratio") {
    x <- lapply(1:mmse@nMPs, function(i) {
      lapply(1:mmse@nsim, function(xx) {
        CBA <- mmse@PPD[[1]][[1]][[i]]@Misc[[xx]][["CBA"]]

        if (!is.null(CBA)) {
          data.frame(Simulation = xx, Year = Year[1:ncol(CBA)], value = CBA[2, ]/CBA[1, ])
        } else {
          data.frame()
        }
      }) %>%
        dplyr::bind_rows() %>%
        mutate(MP = mmse@MPs[[1]][i])
    }) %>%
      dplyr::bind_rows() %>%
      dplyr::filter(!is.na(value))
  }

  xbind <- rbind(x, xhist)

  if (annual) {
    if (type == "F_FMSY") {
      fn <- mean
    } else {
      fn <- sum
    }
    xbind <- xbind %>%
      mutate(Year = floor(Year)) %>%
      group_by(Year, MP, Simulation) %>%
      summarise(value = fn(value, na.rm = TRUE))
  }

  output <- xbind %>%
    dplyr::mutate(OM = .name, MP = factor(MP, levels = mmse@MPs[[1]]))

  return(output)
}

plot_array <- function(MMSE, names, sims,
                       type = c("SB", "B_BMSY", "F_FMSY", "CBA", "Catch_Chile", "Catch_Peru", "Acoustic_Chile", "MPH",
                                "rho", "CBA_ratio", "R"),
                       annual = FALSE) {

  type <- match.arg(type)

  if (is.list(MMSE)) {
    array_df <- Map(.get_array, mmse = MMSE, .name = names, type = type, annual = annual) %>%
      dplyr::bind_rows()

  } else if (is(MMSE, "MMSE")) {
    array_df <- .get_array(MMSE, names, type = type, annual = annual)
  } else {
    stop("MMSE list or object not found.")
  }

  ylab <- switch(type,
                 "SB" = "Biomasa desovante",
                 "B_BMSY" = expression(B/B[RMS]),
                 "F_FMSY" = expression(F/F[RMS]),
                 "CBA" = "CBA",
                 "Catch_Peru" = "Captura peruana",
                 "Catch_Chile" = "Captura chilena",
                 "Acoustic_Chile" = "Crucero acústico",
                 "MPH" = "MPH",
                 "rho" = "rho de Mohn (2do hito)",
                 "CBA_ratio" = expression(CBA[H2]/CBA[H1]),
                 "R" = "Reclutamiento")

  if (!missing(sims)) {

    array_sims <- filter(array_df, Simulation %in% sims) %>%
      mutate(Simulation = factor(Simulation))
    g <- ggplot(array_sims, aes(Year, value, group = Simulation, linetype = Simulation)) +
      facet_grid(vars(MP), vars(OM)) +
      geom_line() +
      expand_limits(y = 0) +
      theme(panel.spacing = unit(0, "in"),
            axis.text.x = element_text(angle = 45, vjust = 0.5),
            legend.position = "bottom",
            strip.background = element_blank()) +
      coord_cartesian(expand = FALSE) +
      labs(x = "Año", y = ylab, linetype = "Simulación")

  } else {
    array_band <- array_df %>%
      group_by(Year, MP, OM) %>%
      summarise(med = median(value),
                lwr = quantile(value, 0.05),
                upr = quantile(value, 0.95),
                lwr2 = quantile(value, 0.25),
                upr2 = quantile(value, 0.75))
    g <- ggplot(array_band, aes(Year, med)) +
      facet_wrap(vars(MP)) +
      #geom_ribbon(fill = "grey90", aes(ymin = lwr, ymax = upr)) +
      #geom_ribbon(fill = "grey70", aes(ymin = lwr2, ymax = upr2)) +
      geom_ribbon(alpha = 0.1, aes(ymin = lwr2, ymax = upr2, fill = OM)) + # Plot only the interquartile range
      geom_line(aes(colour = OM)) +
      expand_limits(y = 0) +
      theme(panel.spacing = unit(0, "in"),
            legend.position = "bottom",
            strip.background = element_blank()) +
      coord_cartesian(expand = FALSE) +
      scale_fill_brewer(palette = "Dark2") +
      scale_colour_brewer(palette = "Dark2") +
      labs(x = "Año", y = ylab, fill = NULL, colour = NULL) +
      guides(fill = guide_legend(ncol = 2),
             colour = guide_legend(ncol = 2))
  }
  if (any(array_df$Year < 2023)) { # 2022 is the historical period
    g <- g + geom_vline(xintercept = 2022.5, linetype = 2)
  }
  g
}



plot_index <- function(MMSE, type = c("MPH", "Chile Acoustic", "Peru Acoustic"),
                       sims) {
  type <- match.arg(type)

  index_text <- switch(
    type,
    "MPH" = "MMSE@PPD[[1]][[1]][[i]]@SpInd",
    "Chile Acoustic" = "MMSE@PPD[[1]][[1]][[i]]@AddInd[, 1, ]",
    "Peru Acoustic" = "MMSE@PPD[[1]][[2]][[i]]@AddInd[, 1, ]"
  )

  index_list <- lapply(1:MMSE@nMPs, function(i) {
    #browser()
    Data <- MMSE@PPD[[1]][[1]][[i]]
    ind <- parse(text = index_text) %>% eval()

    ind %>%
      structure(dimnames = list(Simulation = 1:MMSE@nsim, Year = Data@Year * 0.5 + 1986 - 0.5)) %>%
      reshape2::melt() %>%
      mutate(MP = MMSE@MPs[[1]][i])
  })

  index_df <- do.call(rbind, index_list)

  if (!missing(sims)) {

    if (length(sims) > 2) stop("This function only supports two simulations")

    array_sim <- index_df %>%
      mutate(OM = "OM 1") %>%
      filter(Simulation %in% sims) %>%
      filter(!is.na(value)) %>%
      mutate(Simulation = factor(Simulation))


    g <- ggplot(array_sim, aes(Year, value, shape = Simulation, group = Simulation, linetype = Simulation)) +
      facet_grid(vars(OM), vars(MP)) +
      geom_line() +
      geom_point() +
      expand_limits(y = 0) +
      theme(panel.spacing = unit(0, "in"), strip.background = element_blank()) +
      coord_cartesian(expand = FALSE) +
      labs(y = type) +
      scale_shape_manual(values = c(16, 1))

  } else {
    array_band <- index_df %>%
      mutate(OM = "OM 1") %>%
      group_by(Year, MP, OM) %>%
      summarise(med = median(value, na.rm = TRUE),
                lwr = quantile(value, 0.05, na.rm = TRUE),
                upr = quantile(value, 0.95, na.rm = TRUE),
                lwr2 = quantile(value, 0.25, na.rm = TRUE),
                upr2 = quantile(value, 0.75, na.rm = TRUE)) %>%
      mutate(MP = factor(MP, levels = MMSE@MPs[[1]])) %>%
      filter(!is.na(med))

    g <- ggplot(array_band, aes(Year, med)) +
      facet_grid(vars(OM), vars(MP)) +
      geom_ribbon(fill = "grey90", aes(ymin = lwr, ymax = upr)) +
      geom_ribbon(fill = "grey70", aes(ymin = lwr2, ymax = upr2)) +
      geom_line() +
      expand_limits(y = 0) +
      theme(panel.spacing = unit(0, "in"), strip.background = element_blank()) +
      coord_cartesian(expand = FALSE) +
      labs(y = type)
  }

  g

}


#lapply(MMSE_list, function(mmse) {
#  B0 <- mmse@RefPoint$ByYear$SSB0[, , , 1]   # First year
#  B0x <- mmse@RefPoint$ByYear$SSB0[, , , mmse@nyears] # Last historical year and projection
#  range(B0/B0x - 1)
#})

#lapply(MMSE_list, function(mmse) {
#  F55 <- mmse@RefPoint$ByYear$F_SPR[, "F_55%", 1]   # First year
#  Fx <- mmse@RefPoint$ByYear$F_SPR[, "F_55%", mmse@nyears] # Last historical year and projection
#  range(F55/Fx - 1)
#})

plot_Kobe_time <- function(mmse, output) {

  if (!missing(mmse) && missing(output)) {
    p <- 1 # stock 1
    mp <- 1 # MP 1. No MP changed the selectivity of the fishery so the reference point should be identical for all MPs

    B0 <- mmse@RefPoint$ByYear$SSB0[, p, mp, mmse@nyears]
    B_BMSY <- 2 * mmse@SSB[, p, , ]/B0

    F55 <- mmse@RefPoint$ByYear$F_SPR[, "F_55%", mmse@nyears]
    FM <- apply(mmse@FM, c(1:2, 4:5), sum) # Take the sum of fishing mortality for Chile and Peru
    F_FMSY <- FM[, p, , ]/F55

    Year <- 2023 + 0.5 * seq(0, mmse@proyears - 1)

    Kgreen <- structure(
      B_BMSY >= 0.9 & F_FMSY <= 1.1,
      dimnames = list(Simulation = 1:mmse@nsim, MP = mmse@MPs[[1]], Year = Year)) %>%
      apply(2:3, mean) %>%
      reshape2::melt() %>%
      mutate(val = "green", name = "Plena y sub explotado")

    Kyellow <- structure(
      B_BMSY >= 0.9 & F_FMSY > 1.1,
      dimnames = list(Simulation = 1:mmse@nsim, MP = mmse@MPs[[1]], Year = Year)) %>%
      apply(2:3, mean) %>%
      reshape2::melt() %>%
      mutate(val = "yellow", name = "Sobreexplotado (por pesca)")

    Korange <- structure(
      B_BMSY >= 0.5 & B_BMSY < 0.9,
      dimnames = list(Simulation = 1:mmse@nsim, MP = mmse@MPs[[1]], Year = Year)) %>%
      apply(2:3, mean) %>%
      reshape2::melt() %>%
      mutate(val = "orange", name = "Sobreexplotado")

    Kred <- structure(
      B_BMSY < 0.5,
      dimnames = list(Simulation = 1:mmse@nsim, MP = mmse@MPs[[1]], Year = Year)) %>%
      apply(2:3, mean) %>%
      reshape2::melt() %>%
      mutate(val = "red", name = "Agotamiento")

    output <- rbind(Kgreen, Korange, Kyellow, Kred)
  }

  fill_values = c("Plena y sub explotado" = "green",
                  "Sobreexplotado (por pesca)" = "yellow",
                  "Sobreexplotado" = "orange",
                  "Agotamiento" = "red")

  g <- output %>%
    mutate(name = factor(name, levels = names(fill_values))) %>%
    ggplot(aes(Year, value)) +
    geom_col(aes(fill = name), width = 0.5, colour = NA) +
    facet_wrap(vars(MP)) +
    scale_fill_manual(values = fill_values) +
    labs(x = "Año", y = "Probabilidad", fill = NULL) +
    theme(legend.position = "bottom",
          strip.background = element_blank()) +
    guides(fill = guide_legend(ncol = 2)) +
    coord_cartesian(expand = FALSE)
  g
}

convertMSE <- function(mmse) {
  p <- 1
  f <- 1

  MSE <- new("MSE", Name = mmse@Name, nyears = mmse@nyears, proyears = mmse@proyears,
             MPs = mmse@MPs[[1]], nMPs = mmse@nMPs, nsim = mmse@nsim, OM = mmse@OM[[1]][[1]], Obs = data.frame(),
             SB_SBMSY = mmse@SB_SBMSY[, p, , ],
             F_FMSY = apply(mmse@FM[, 1, , , ], c(1, 3, 4), sum)/mmse@RefPoint$ByYear$F_SPR[, "F_55%", mmse@nyears],
             N = mmse@N[, p, , , , ],
             SSB = mmse@SSB[, p, , ],
             B = mmse@B[, p, , ],
             VB = mmse@VB[, p, , ],
             FM = apply(mmse@FM[, 1, , , ], c(1, 3, 4), sum),
             SPR = mmse@SPR,
             Catch = matrix(),
             Removals = matrix(),
             Effort = matrix(),
             TAC = matrix(),
             TAE = matrix(),
             BioEco = list(),
             RefPoint = mmse@RefPoint,
             CB_hist = matrix(),
             FM_hist = sapply(mmse@multiHist[[1]], function(i) i@SampPars$Fleet$qs * i@SampPars$Fleet$Find, simplify = "array") %>%
               apply(1:2, sum),
             SSB_hist = apply(mmse@multiHist[[p]][[1]]@TSdata$SBiomass, 1:2, sum),
             Hist = new("Hist"),
             PPD = mmse@PPD[[p]][[f]],
             Misc = list())
  return(MSE)
}

plot_diagnostic <- function(mmse, MP = c("PM_actual", "PM_rho"), figure = TRUE) {
  MP <- match.arg(MP)
  MSE <- convertMSE(mmse)
  SAMtool::diagnostic(MSE, MP, figure = figure)
}

plot_ret_AM <- function(mmse, MP = c("PM_actual", "PM_rho"), sim = 1) {
  MP <- match.arg(MP)
  MSE <- convertMSE(mmse)
  SAMtool::retrospective_AM(MSE, MP, sim = sim)
}

