# ------------------------------------------------------------------------
# Script for SC12---------------------------------------------------------
# 2024 JM  ---------------------------------------------------------------
# ------------------------------------------------------------------------
#install.packages("devtools")
#
# MAKE SURE TO UPDATE
# devtools::install_github("sprfmo/jjmr")
# Remember to recompile jjms as needed in the src folder

library(jjmR)
library(tidyverse)


#---If running models in parallel

library(foreach)
library(doParallel)

ncores <- 2
cl <- if(Sys.info()[["sysname"]]=="Windows") makePSOCKcluster(ncores) else ncores
registerDoParallel(cl)

#--------------------------------------------------------
# Working directory should be in assessment folder of jjm
#--------------------------------------------------------
# Changes to both data and control file
fn_update <- function(newmod, newmodname, h, dodat=T) {

  names(newmod) <- newmod[[1]]$control$modelName <- geth(newmodname,h)

  if(dodat) newmod[[1]]$control$dataFile <- paste0(newmodname,".dat")

  writeJJM(newmod,datPath="input",ctlPath="config")
}

# Only changes to data file
fn_bridge <- function(newmod, newmodname, h2mod=h2_ctl,ln_dat=line_dat, ln_modnm=line_modnm) {

  names(newmod) <- newmod[[1]]$control$modelName <- geth(newmodname,"h1")
  newmod[[1]]$control$dataFile <- h2mod[ln_dat] <- paste0(newmodname,".dat")

  h2mod[ln_dat] <- paste0(newmodname, ".dat")
  h2mod[ln_modnm] <- geth(newmodname, "h2")

  writeJJM(newmod,datPath="input",ctlPath="config")
  writeLines(h2mod, con=paste0("config/h2_",newmodname,".ctl"))
}

h2_ctl <- readLines("config/h2_1.00.ctl")
line_dat <- grep("dataFile", h2_ctl) + 1
line_modnm <- grep("modelName", h2_ctl) + 1

#-------------------------
# Model configuration runs
#-------------------------

# Bridging now done in SC12_Bridging.R

#--------------------
# Full model
#--------------------

# mod1.00 <- runit(geth("1.00"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

h1_1.00 <- readJJM(geth("1.00","h1"), path = "config", input = "input")
h2_1.00 <- readJJM(geth("1.00","h2"), path = "config", input = "input")

file_dat <- h1_1.00[[1]]$data

finmodname <- "1.07"

tac_prev <- sum(tail(h1_1.00[[1]]$data$Fcaton,1))

#----------
# Changing catchability for offshore CPUE in 2021
#----------
h1_1.01 <- h1_1.00
h2_1.01 <- h2_1.00

i <- grep("Offshore", h1_1.01[[1]]$data$Inames)
f <- grep("Offshore", h1_1.01[[1]]$data$Fnames)


# Downweight selectivity change penalty (increase flexibility) in 2021
ff <- which(h1_1.01[[1]]$control[[paste0("F",f,"_","selchangeYear")]]==2021)
h1_1.01[[1]]$control[[paste0("F",f,"_","selchange")]][ff] <- h2_1.01[[1]]$control[[paste0("F",f,"_","selchange")]][ff] <- .8

ff <- which(h1_1.01[[1]]$control[[paste0("F",f,"_","selchangeYear")]]==2022)
h1_1.01[[1]]$control[[paste0("F",f,"_","selchange")]][ff] <- h2_1.01[[1]]$control[[paste0("F",f,"_","selchange")]][ff] <- .5

# Add break in q
h1_1.01[[1]]$control$RW_q_phases[i] <- h2_1.01[[1]]$control$RW_q_phases[i] <- 1 # Make sure it's estimated
h1_1.01[[1]]$control$RW_nyrs_q[i] <- h2_1.01[[1]]$control$RW_nyrs_q[i] <- h1_1.01[[1]]$control$RW_nyrs_q[i]+1  # Add number of years for change
h1_1.01[[1]]$control$RW_q_yrs <- h2_1.01[[1]]$control$RW_q_yrs <- c(h1_1.01[[1]]$control$RW_q_yrs, 2021) # Add year of change
h1_1.01[[1]]$control$RW_q_sigmas <- h2_1.01[[1]]$control$RW_q_sigmas <- c(h1_1.01[[1]]$control$RW_q_sigmas, h1_1.01[[1]]$control$RW_q_sigmas[1]) # Sigma for random walk

# Change CV back to usual
CV_CPUE <- 0.2
y <- which(rownames(h1_1.01[[1]]$data$Index) == 2022)
h1_1.01[[1]]$data$Indexerr[y,i] <- h2_1.01[[1]]$data$Indexerr[y,i] <- h1_1.01[[1]]$data$Index[y,i] * CV_CPUE

fn_update(h1_1.01, "1.01", "h1")
fn_update(h2_1.01, "1.01", "h2")
mod_1.01 <- runit(geth("1.01",c("h1","h2")),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm", parallel=TRUE, adflags=paste0("-tac ", tac_prev))

#----------
# New CPUE index proposed in JM07
# Using regular CPUE CV of 0.2
#----------
h1_1.02 <- h1_1.00
chile_cpue <- read_csv(here::here("data","sc12-jm06.csv")) %>%
                janitor::clean_names() %>%
                mutate(sd=mean-low,
                        cv=sd/mean)

cv_cpue <- 0.2

i <- grep("Chile_CPUE",h1_1.02[[1]]$data$Inames)
rows2use <- which(rownames(h1_1.02[[1]]$data$Index) %in% chile_cpue$year)

h1_1.02[[1]]$data$Inumyears[i]          <- dim(chile_cpue)[1]
h1_1.02[[1]]$data$Index[,i]             <- h1_1.02[[1]]$data$Indexerr[,i] <- h1_1.02[[1]]$data$Iyears[,i] <- NA # In case the years change.
h1_1.02[[1]]$data$Index[rows2use,i]     <- chile_cpue$mean
h1_1.02[[1]]$data$Indexerr[rows2use,i]  <- chile_cpue$mean * cv_cpue
h1_1.02[[1]]$data$Iyears[rows2use,i]    <- chile_cpue$year

fn_bridge(h1_1.02, "1.02")
mod_1.02 <- runit(geth("1.02",c("h1","h2")),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm", parallel=TRUE, adflags=paste0("-tac ", tac_prev))


#----------
# New CPUE index proposed in JM07
# Using regular CPUE CV of 0.2
#----------
h1_1.03 <- h1_1.02

h1_1.03[[1]]$data$Indexerr[rows2use,i]    <- chile_cpue$sd

fn_bridge(h1_1.03, "1.03")
mod_1.03 <- runit(geth("1.03",c("h1","h2")),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm", parallel=TRUE, adflags=paste0("-tac ", tac_prev))


#----------
# Updating Peruvian wt at age
# Data file provided by Criscely 12/09/23
#----------

h1_1.06 <- h1_1.00
h2_1.06 <- h2_1.00

wtatage_peru <- read_csv(here::here("data","wtatage_peru.csv"))

f <- grep("FarNorth", h1_1.06[[1]]$data$Fnames)
i <- grep("Peru_CPUE",h1_1.06[[1]]$data$Inames)

rows2use <- which(rownames(h1_1.06[[1]]$data$Fwtatage[,,f]) %in% wtatage_peru$year)
dat2use <- wtatage_peru %>%
            select(-year) %>%
            as.matrix()

h1_1.06[[1]]$data$Fwtatage[rows2use,,f] <- h2_1.06[[1]]$data$Fwtatage[rows2use,,f] <- dat2use

# Update wtatage for CPUE
rows2use <- which(rownames(h1_1.06[[1]]$data$Iwtatage[,,i]) %in% wtatage_peru$year)
h1_1.06[[1]]$data$Iwtatage[rows2use,,i] <- h2_1.06[[1]]$data$Iwtatage[rows2use,,i] <- dat2use

# fn_update(h1_1.06, "1.06", "h1", dodat=T)
# fn_update(h2_1.06, "1.06", "h2", dodat=T)
h1_1.06 <- runit("h1_1.06",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
h2_1.06 <- runit("h2_1.06",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")

#----------
# Downweight last value for offshore CPUE
#----------

h1_1.07 <- h1_1.06

i <- grep("Offshore",h1_1.07[[1]]$data$Inames)
rows2use <- which(h1_1.07[[1]]$data$Iyears[,i]==max(h1_1.07[[1]]$data$Iyears[,i],na.rm=T))

h1_1.07[[1]]$data$Indexerr[rows2use,i] <- h1_1.00[[1]]$data$Indexerr[rows2use,i] * 2

# fn_bridge(h1_1.07, "1.07",h2mod=readLines("config/h2_1.06.ctl"))
h_1.07 <- runit(c("h1_1.07","h2_1.07"),parallel=TRUE,pdf=TRUE,portrait=F,est=F,exec="../src/jjm")

#----------
# Move Peruvian catch in high seas from Fleet 3 to Fleet 4
# 20,056 tons
#----------

h1_1.08 <- h1_1.07 <- readJJM("h1_1.07",path="config",input="input")

catch2move <- 20056/1e3
catch_fin <- h1_1.07[[1]]$data$Fcaton[which(rownames(h1_1.07[[1]]$data$Fcaton)==2023),]
catch_fin[3] <- catch_fin[3]-catch2move
catch_fin[4] <- catch_fin[4]+catch2move
h1_1.08[[1]]$data$Fcaton[which(rownames(h1_1.08[[1]]$data$Fcaton)==2023),] <- catch_fin

fn_bridge(h1_1.08, "1.08", h2mod=readLines("config/h2_1.06.ctl"))
# h1_1.08 <- runit("h1_1.08",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
# h2_1.08 <- runit("h2_1.08",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")


#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# Projection runs
#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# Now done in run_projections.R

#--------------------------
# Tables for the Tech Annex
# Juan Carlos Quiroz's tables
# Loads packages
#--------------------------

source("annex/constructor_input_tables.R")

construct_input_tables(modname=finmodname,docname="annex/Annex_Tables")

# Summary data
finmod <- readJJM(geth(finmodname,"h1"),path="config",input="input")
write.csv(summary(finmod)$like,"results/SummaryLikelihoods_h1.csv")

finmod <- readJJM(geth(finmodname,"h2"),path="config",input="input")
write.csv(summary(finmod)$like,"results/SummaryLikelihoods_h2.csv")

#------------------
# Kobe for main doc
# Summary sheets
#------------------
kobe(finmod)
kobe(fixed_bmsy(finmod) ,add=T, col = 'red')

main_diag <- diagnostics(fixed_bmsy(finmod),plots=F)
plot(main_diag, var = "summarySheet")

main_diag <- diagnostics(finmod,plots=F)
plot(main_diag, var = "summarySheet")
