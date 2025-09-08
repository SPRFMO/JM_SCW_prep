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
# Leave in Peru CPUE
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
# Leave in Acoustic N Chile
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
# Leave in Acoustic N Chile
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
