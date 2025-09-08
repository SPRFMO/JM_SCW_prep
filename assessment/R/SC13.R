# ------------------------------------------------------------------------
# Script for SC13---------------------------------------------------------
# 2025 JM  ---------------------------------------------------------------
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
fn_bridge <- function(newmod, newmodname, h2mod, ln_dat=line_dat, ln_modnm=line_modnm) {

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


#-------------------
# 1.01 Leave in Chile CPUE
#-------------------

mod_new <- h1_1.00

dw <- c("Chile_AcousN", "Offshore_CPUE", "Peru_CPUE")
vec_i <- which(mod_new[[1]]$data$Inames %in% dw)

for(i in vec_i) {
  rows2use <- !is.na(mod_new[[1]]$data$Index[,i])
  mod_new[[1]]$data$Indexerr[rows2use, i] <- h1_1.00[[1]]$data$Indexerr[rows2use, i] * 100
}

fn_bridge(mod_new, "1.01", h2mod = h2_ctl)
mod1.01 <- runit(geth("1.01",c("h1","h2")),pdf=T,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)


#-------------------
# 1.02 Leave in Offshore CPUE
#-------------------

mod_new <- h1_1.00

dw <- c("Chile_AcousN", "Chile_CPUE", "Peru_CPUE")
vec_i <- which(mod_new[[1]]$data$Inames %in% dw)

for(i in vec_i) {
  rows2use <- !is.na(mod_new[[1]]$data$Index[,i])
  mod_new[[1]]$data$Indexerr[rows2use, i] <- h1_1.00[[1]]$data$Indexerr[rows2use, i] * 100
}

fn_bridge(mod_new, "1.02", h2mod = h2_ctl)
mod1.02 <- runit(geth("1.02",c("h1","h2")),pdf=T,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)


#-------------------
# 1.03 Leave in Peru CPUE
#-------------------

mod_new <- h1_1.00

dw <- c("Chile_AcousN", "Offshore_CPUE", "Chile_CPUE")
vec_i <- which(mod_new[[1]]$data$Inames %in% dw)

for(i in vec_i) {
  rows2use <- !is.na(mod_new[[1]]$data$Index[,i])
  mod_new[[1]]$data$Indexerr[rows2use, i] <- h1_1.00[[1]]$data$Indexerr[rows2use, i] * 100
}

fn_bridge(mod_new, "1.03", h2mod = h2_ctl)
mod1.03 <- runit(geth("1.03",c("h1","h2")),pdf=T,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)


#-------------------
# 1.04 Leave in Acoustic N Chile
#-------------------

mod_new <- h1_1.00

dw <- c("Peru_CPUE", "Offshore_CPUE", "Chile_CPUE")
vec_i <- which(mod_new[[1]]$data$Inames %in% dw)

for(i in vec_i) {
  rows2use <- !is.na(mod_new[[1]]$data$Index[,i])
  mod_new[[1]]$data$Indexerr[rows2use, i] <- h1_1.00[[1]]$data$Indexerr[rows2use, i] * 100
}

fn_bridge(mod_new, "1.04", h2mod = h2_ctl)
mod1.04 <- runit(geth("1.04",c("h1","h2")),pdf=T,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)


#-------------------
# 1.05 Leave in Acoustic N Chile
# Downweight earlier years
#-------------------

mod_new <- h1_1.00

dw <- c("Peru_CPUE", "Offshore_CPUE", "Chile_CPUE")
vec_i <- which(mod_new[[1]]$data$Inames %in% dw)

for(i in vec_i) {
  rows2use <- !is.na(mod_new[[1]]$data$Index[,i])
  mod_new[[1]]$data$Indexerr[rows2use, i] <- h1_1.00[[1]]$data$Indexerr[rows2use, i] * 100
}

i <- which(mod_new[[1]]$data$Inames == "Chile_AcousN")
rows2use <- which(rownames(mod_new[[1]]$data$Index) < 2000)
mod_new[[1]]$data$Indexerr[rows2use, i] <- h1_1.00[[1]]$data$Indexerr[rows2use, i] * 100

fn_bridge(mod_new, "1.05", h2mod = h2_ctl)
mod1.05 <- runit(geth("1.05",c("h1","h2")),pdf=T,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)


#-------------------
# 1.06 Leave out Chile CPUE
#-------------------

mod_new <- h1_1.00

dw <- c("Chile_CPUE")
i <- which(mod_new[[1]]$data$Inames %in% dw)

rows2use <- !is.na(mod_new[[1]]$data$Index[,i])
mod_new[[1]]$data$Indexerr[rows2use, i] <- h1_1.00[[1]]$data$Indexerr[rows2use, i] * 100

fn_bridge(mod_new, "1.06", h2mod = h2_ctl)
mod1.06 <- runit(geth("1.06",c("h1","h2")),pdf=T,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)


#-------------------
# 1.07 Leave out Offshore CPUE
#-------------------

mod_new <- h1_1.00

dw <- c("Offshore_CPUE")
i <- which(mod_new[[1]]$data$Inames %in% dw)

rows2use <- !is.na(mod_new[[1]]$data$Index[,i])
mod_new[[1]]$data$Indexerr[rows2use, i] <- h1_1.00[[1]]$data$Indexerr[rows2use, i] * 100

fn_bridge(mod_new, "1.07", h2mod = h2_ctl)
mod1.07 <- runit(geth("1.07",c("h1","h2")),pdf=T,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)


#-------------------
# 1.08 Leave out Peru CPUE
#-------------------

mod_new <- h1_1.00

dw <- c("Peru_CPUE")
i <- which(mod_new[[1]]$data$Inames %in% dw)

rows2use <- !is.na(mod_new[[1]]$data$Index[,i])
mod_new[[1]]$data$Indexerr[rows2use, i] <- h1_1.00[[1]]$data$Indexerr[rows2use, i] * 100

fn_bridge(mod_new, "1.08", h2mod = h2_ctl)
mod1.08 <- runit(geth("1.08",c("h1","h2")),pdf=T,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)



#-------------------
# 1.09 Leave out Acoustic N Chile
#-------------------

mod_new <- h1_1.00

dw <- c("Chile_AcousN")
i <- which(mod_new[[1]]$data$Inames %in% dw)

rows2use <- !is.na(mod_new[[1]]$data$Index[,i])
mod_new[[1]]$data$Indexerr[rows2use, i] <- h1_1.00[[1]]$data$Indexerr[rows2use, i] * 100

fn_bridge(mod_new, "1.09", h2mod = h2_ctl)
mod1.09 <- runit(geth("1.09",c("h1","h2")),pdf=T,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)



#-------------------
# 1.10 Changing selectivity and catchability for offshore fleet in 2025
#-------------------


h1_1.10 <- h1_1.00
h2_1.10 <- h2_1.00

yr2change <- 2025

i <- grep("Offshore", h1_1.10[[1]]$data$Inames)
f <- grep("Offshore", h1_1.10[[1]]$data$Fnames)


# Downweight selectivity change penalty (increase flexibility) in 2021
ff <- which(h1_1.10[[1]]$control[[paste0("F",f,"_","selchangeYear")]]==2021)
h1_1.01[[1]]$control[[paste0("F",f,"_","selchange")]][ff] <- h2_1.01[[1]]$control[[paste0("F",f,"_","selchange")]][ff] <- .8

# Add break in q
h1_1.10[[1]]$control$RW_q_phases[i] <- h2_1.01[[1]]$control$RW_q_phases[i] <- 1 # Make sure it's estimated
h1_1.10[[1]]$control$RW_nyrs_q[i] <- h2_1.01[[1]]$control$RW_nyrs_q[i] <- h1_1.01[[1]]$control$RW_nyrs_q[i]+1  # Add number of years for change
h1_1.10[[1]]$control$RW_q_yrs <- h2_1.01[[1]]$control$RW_q_yrs <- c(h1_1.01[[1]]$control$RW_q_yrs, 2021) # Add year of change
h1_1.10[[1]]$control$RW_q_sigmas <- h2_1.01[[1]]$control$RW_q_sigmas <- c(h1_1.01[[1]]$control$RW_q_sigmas, h1_1.01[[1]]$control$RW_q_sigmas[1]) # Sigma for random walk

# Change CV back to usual
cv_cpue <- 0.2
y <- which(rownames(h1_1.01[[1]]$data$Index) == 2022)
h1_1.10[[1]]$data$Indexerr[y,i] <- h2_1.01[[1]]$data$Indexerr[y,i] <- h1_1.01[[1]]$data$Index[y,i] * cv_cpue

fn_update(h1_1.10, "1.10", "h1")
fn_update(h2_1.10, "1.10", "h2")
mod_1.10 <- runit(geth("1.10",c("h1","h2")),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm", parallel=TRUE, adflags=paste0("-tac ", tac_prev))


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
# kobe(finmod)
# kobe(fixed_bmsy(finmod) ,add=T, col = 'red')

# main_diag <- diagnostics(fixed_bmsy(finmod),plots=F)
# plot(main_diag, var = "summarySheet")

# main_diag <- diagnostics(finmod,plots=F)
# plot(main_diag, var = "summarySheet")
