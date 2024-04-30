# ===========================================================================================================
# === Script to summarise MSE results in a Slick object =====================================================
# ===========================================================================================================

# Tom Carruthers
# March 2024

# This is designed as a source script

# --- Installation -------------------------------------------------------------

# devtools::install_github('blue-matter/slick')

# --- Prerequisites ------------------------------------------------------------

setwd("~/_mymods/SPRFMO/jjm/assessment")
# setwd("C:/GitHub/jjm/assessment")          # Tom WD

library(openMSE)
library(Slick)
library(dplyr)
library(ggpubr)

largedir = "C:/temp/JM_mcmc/"                # somewhere to store big things off the github 

source("../mse/Z - performance metric functions.R") # source some performance metrics (same in principle as anchovy)
# ZV zone verde (prob. green Kobe)
# NSE no sobre exploitado (prob. above 90% SSBMSY)
# NZR no zona roja (probability not red Kobe)
# CBAprom  CBA promedio 
# NSP no sobre pesa (prob. F < 1.1 FMSY)
# year derivatives: CP(1 - 5), MP(6-15), LP(16-30) 


# Get MMSE objects, copy from script 5 ----

nOM = 4
OM_names <- c("(1) hypoth 1, steepness 0.65, short rec",
              "(2) hypoth 1, steepness 0.65, long rec",
              "(3) hypoth 1, steepness 0.90, short rec",
              "(4) hypoth 1, steepness 0.90, long rec")

codes = paste("h1",c("ls","ll","hs","hl"),sep="_")
OM_short <- paste(1:4,codes,sep="_")

MMSE_list <- lapply(1:nOM, function(i) {
  mmse <- readRDS(file = paste0(largedir,"MSEs/MSE_",codes[i], ".rda"))
  
  Hist <- readRDS(file = paste0(largedir,"Hists/Hist_",codes[i], ".rda"))
  
  # Append F55 to mmse object
  mmse@RefPoint$ByYear$F_SPR <- Hist[[1]][[1]]@Ref$ByYear$F_SPR
  
  # Append historical object
  mmse@multiHist <- Hist
  mmse
  
})


# Performance metrics, copy from script Z ----

PMs = c("NSE_LP","NSP_LP","ZV_LP","CBA_CP","CBA_MP","CBA_LP")
PMlabs = c("P(B>BMSY)","P(F<1.1FMSY)","P(GK)","Y(1-5)","Y(6-15)","Y(16-30)") 
PMlabs_det = c("P(B>BMSY)","P(F<1.1FMSY)","P(GK)","Y(1-5)(%max)","Y(6-15)(%max)","Y(16-30)(%max)") 



# Make Slick object ----

StateVar <- c("B/BMSY",
              "F/FMSY",
              "Yield",
              "Spawning biomass",
               "Annual Recruitment")

obj <- Slick::NewSlick(
  name = "Pacific Jack Mackerel (demo)",
  nPerf = list(nD = length(PMs), # 19
               nS = length(PMs),
               nP = 2),
  nMPs = MMSE_list[[1]]@nMPs, #7
  nsim = MMSE_list[[1]]@nsim, #100
  nProjYr = MMSE_list[[1]]@proyears, # 30 semesters = 15 real years
  nStateVar = length(StateVar),
  nHistYr = MMSE_list[[1]]@nyears, # 74 semesters = 37 real years
  Design = expand.grid(1:2, 1:2)
)

## OM design ----
obj$OM$Factor_Labels <- c("SRR Duration", "Steepness")
obj$OM$Description <- list(c("SRR short time period", "SRR long time period"), c("Less resilient (B.H. steepness = 0.65)", "Resilient (B.H. steepness = 0.9)"))

obj$OM$Codes <-
  obj$OM$Labels <- list(c("SRR short", "SRR long"), c("0.65", "0.90"))

## Management procedures ----
obj$MP$Labels <- obj$MP$Codes <- MMSE_list[[1]]@MPs[[1]]
obj$MP$Description <- c("Index ratio, tuning parameter 1, with HCR",
                        "Index ratio, tuning parameter 1, no HCR",
                        "Index ratio, tuning parameter 2, with HCR",
                        "Index ratio, tuning parameter 2, no HCR",
                        "Index ratio, tuning parameter 3, with HCR",
                        "Index ratio, tuning parameter 3, no HCR")

## Deterministic performance metrics ----
## Need to multiply by 100
obj$Perf$Det$Labels <-
  obj$Perf$Det$Codes <- PMlabs_det # PMs = c("NSE_LP","NSP_LP","ZV_LP","CBA_CP","CBA_MP","CBA_LP")

obj$Perf$Det$Description <- paste(sapply(PMs, function(i) get(i) %>% formals() %>% getElement("Name")),c(rep("",3),rep("(%max)",3)))


PMobj <- lapply(MMSE_list, PM_fn, PMs = PMs)
for (i in 1:length(MMSE_list)) {
  obj$Perf$Det$Values[i, , ] <- as.matrix(PMobj[[i]][, 1:length(PMs)])
}
Catmets = obj$Perf$Det$Values[, , grepl("CBA", PMs)]
maxoverMPs = apply(Catmets,c(1,3),max)
for(i in 1:MMSE_list[[1]]@nMPs)obj$Perf$Det$Values[, i, grepl("CBA", PMs)] = obj$Perf$Det$Values[, i, grepl("CBA", PMs)]/maxoverMPs
obj$Perf$Det$Values[] <- 100 * obj$Perf$Det$Values[]


## Stochastic performance metrics ----
obj$Perf$Stoch$Labels <-
  obj$Perf$Stoch$Codes <- PMlabs
obj$Perf$Stoch$Description <- sapply(PMs, function(i) get(i) %>% formals() %>% getElement("Name"))

PMsim <- lapply(MMSE_list, PM_fn, PMs = PMs, all_sims = TRUE)
obj$Perf$Stoch$Values[] <- simplify2array(PMsim) %>% aperm(c(1, 4, 2, 3))
obj$Perf$Stoch$Values[, , , !grepl("CBAprom", PMs)] <- 100 * obj$Perf$Stoch$Values[, , , !grepl("CBAprom", PMs)]


## Projection performance metrics ----
obj$Perf$Proj$Labels <- obj$Perf$Proj$Codes <- obj$Perf$Proj$Description <- StateVar[1:3]

allYear <- seq(1970, 2073, 1)
last_hist <- 2023

source("../mse/ZZ - functions-figures.R")
nsim = MMSE_list[[1]]@nsim
df2matrix <- function(x) reshape2::acast(x, list("Simulation", "OM", "MP", "Year"))
B_BMSY <- plot_array(MMSE_list, 1:4, sims = 1:nsim, type = "B_BMSY")[["data"]] %>%
  df2matrix()
F_FMSY <- plot_array(MMSE_list, 1:4, sims = 1:nsim, type = "F_FMSY")[["data"]] %>%
  df2matrix()
F_FMSY_annual <- plot_array(MMSE_list, 1:4, sims = 1:nsim, type = "F_FMSY", annual = TRUE)[["data"]] %>%
  df2matrix()


obj$Perf$Proj$Times <- allYear[allYear > last_hist]

obj$Perf$Proj$Values[, , , 1, ] <- B_BMSY[, , , allYear > last_hist]
obj$Perf$Proj$Values[, , , 2, ] <- F_FMSY[, , , allYear > last_hist]

obj$Perf$Proj$RefPoints <- list(c(0.5, 0.9), 1.1)
obj$Perf$Proj$RefNames <- list(c("Limit", "Target"), "Limit")




## State variables ----
CBA <- plot_array(MMSE_list, 1:4, sims = 1:nsim, type = "CBA", annual = TRUE)[["data"]] %>%
  df2matrix()

SB <- plot_array(MMSE_list, 1:4, sims = 1:nsim, type = "SB")[["data"]] %>%
  df2matrix()


#CC <- plot_array(MMSE_list, 1:4, sims = 1:nsim, type = "Catch_Chile", annual = TRUE)[["data"]] %>%
#  df2matrix()

#CP <- plot_array(MMSE_list, 1:4, sims = 1:nsim, type = "Catch_Peru", annual = TRUE)[["data"]] %>%
 # df2matrix()

R <- plot_array(MMSE_list, 1:4, sims = 1:nsim, type = "R")[["data"]] %>%
  df2matrix()


obj$StateVar$Times <- allYear
obj$StateVar$TimeNow <- 2023
obj$StateVar$Labels <- obj$StateVar$Description <- obj$StateVar$Codes <- StateVar


obj$StateVar$Values[] <- 0
obj$StateVar$Values[, , , 1, ] <- B_BMSY
obj$StateVar$Values[, , , 2, ] <- F_FMSY

#obj$StateVar$Values[, , , 3, seq(1, 104, 2)] <-
#  obj$StateVar$Values[, , , 3, seq(2, 104, 2)] <- df2matrix(F_FMSY_annual)

pind = MMSE_list[[1]]@nyears+1:MMSE_list[[1]]@proyears
obj$StateVar$Values[, , , 3, pind] <-
  obj$StateVar$Values[, , , 3, pind] <- CBA

obj$StateVar$Values[, , , 4, ] <- SB

#obj$StateVar$Values[, , , 6, seq(1, 104, 2)] <-
#  obj$StateVar$Values[, , , 6, seq(2, 104, 2)] <- CC

#obj$StateVar$Values[, , , 7, seq(1, 104, 2)] <-
#  obj$StateVar$Values[, , , 7, seq(2, 104, 2)] <- CP

obj$StateVar$Values[, , , 5, ] <- R

obj$StateVar$RefPoints <- list(c(0.5, 0.9), 1.1, NULL, NULL, NULL)
obj$StateVar$RefNames <- list(c("Limit", "Target"), "Limit", NULL, NULL, NULL)



## Text ----
obj$Text$Title <- "SPRFMO Jack Mackerel (demo)"
obj$Text$Sub_title <- "Demonstration of openMSE results March 2024 using J. Ianelli's conditioning model"


## Contact information ----
obj$Misc$Author <- "T. Carruthers & J. Ianelli"
obj$Misc$Contact <- "(tom@bluematterscience.com)"
obj$Misc$Institution <- "Blue Matter Science"

obj$Misc$App_axes # Can translate into Spanish?
obj$Misc$App_axes_code # Can translate into Spanish?

## Save object ----
saveRDS(obj, file = "../mse/Slick/JM_Demo_Mar_2024 v2.slick")

## Open Slick ----
Slick::Slick()



# === End of script ===========================================================================



