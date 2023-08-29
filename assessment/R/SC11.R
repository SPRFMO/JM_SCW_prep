# ------------------------------------------------------------------------
# Script for SC11---------------------------------------------------------
# 2023 JM  ---------------------------------------------------------------
# ------------------------------------------------------------------------
#install.packages("devtools")
#
# MAKE SURE TO UPDATE
# devtools::install_github("sprfmo/jjmr")
# Remember to recompile jjms as needed in the src folder

library(jjmR)
library(tidyverse)

#--------------------------------------------------------
# Working directory should be in assessment folder of jjm
#--------------------------------------------------------
# setwd(file.path(getwd(), "assessment"))
pwd <- getwd()
if (!grepl(basename(pwd), "assessment", ignore.case = TRUE)) {
  stop(paste("Set working directory to jjm/assessment"))
}
geth <- function(mod,h=hyp) paste0(h,"_", mod) # Package? Or keep?

# Function to put a constant reference point into image
fixed_bmsy <- function(mod, refpt=T){
  if(refpt) refpt <- mean(rev(mod[[1]]$output[[1]]$msy_mt[,10])[1:10])
  old_rat <- (mod[[1]]$output[[1]]$msy_mt[,13])
  new_rat <- (mod[[1]]$output[[1]]$msy_mt[,12]/ refpt)
  mod[[1]]$output[[1]]$msy_mt[,13] <- new_rat
  mod[[1]]$output[[1]]$msy_mt[,10] <- refpt
  return(mod)
}

fn_update <- function(newmod, newmodname, h, dodat=T) {

  names(newmod) <- newmod[[1]]$control$modelName <- geth(newmodname,h)

  if(dodat) newmod[[1]]$control$dataFile <- paste0(newmodname,".dat")
  
  writeJJM(newmod,datPath="input",ctlPath="config")
}

#-------------------------
# Model configuration runs
#-------------------------

# Bridging now done in SC10_Bridging.R

#--------------------
# Full model
#--------------------

# mod1.00 <- runit(geth("1.00"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

h1_1.00 <- readJJM(geth("1.00","h1"), path = "config", input = "input")
h2_1.00 <- readJJM(geth("1.00","h2"), path = "config", input = "input")

file.dat <- h1_1.00[[1]]$data

FinModName <- "1.02"

#----------
# Correcting growth parameters
#----------

h1_1.01 <- h1_1.00
h1_1.01[[1]]$data <- file.dat
h2_1.01 <- h2_1.00
h2_1.01[[1]]$data <- file.dat

h1_1.01[[1]]$control$Linf[1,1] <- h2_1.01[[1]]$control$Linf[1,] <- 73.56
h1_1.01[[1]]$control$Lo_len[1,1] <- h2_1.01[[1]]$control$Lo_len[1,] <- 13.56

# fn_update(h1_1.01, "1.01", "h1", dodat=F)
# fn_update(h2_1.01, "1.01", "h2", dodat=F)
# h1_1.01 <- runit("h1_1.01",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# h2_1.01 <- runit("h2_1.01",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

#----------
# Adding more flexibility in selectivity for offshore fleet
#----------

h1_1.01 <- readJJM("h1_1.01", path = "config", input = "input")
h2_1.01 <- readJJM("h2_1.01", path = "config", input = "input")

h1_1.02 <- h1_1.01
h2_1.02 <- h2_1.01

# Increase curve penalty for final year
h1_1.02[[1]]$control$F4_selchange[length(h1_1.02[[1]]$control$F4_selchange)] <- h2_1.02[[1]]$control$F4_selchange[length(h2_1.02[[1]]$control$F4_selchange)]  <- 0.8

# Increase sample size for final year age comp
h1_1.02[[1]]$data$Fagesample[dim(h1_1.02[[1]]$data$Fagesample)[1],4] <- h2_1.02[[1]]$data$Fagesample[dim(h2_1.02[[1]]$data$Fagesample)[1],4] <- h2_1.02[[1]]$data$Fagesample[(dim(h2_1.02[[1]]$data$Fagesample)[1]-1),4]

# fn_update(h1_1.02, "1.02", "h1", dodat=T)
# fn_update(h2_1.02, "1.02", "h2", dodat=T)
# h1_1.02 <- runit("h1_1.02",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# h2_1.02 <- runit("h2_1.02",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# Projection runs
#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# Now done in run_projections.R

#--------------------------
# Tables for the Tech Annex
# Juan Carlos Quiroz's tables
# Loads packages
#--------------------------

source("R/constructor_input_tables.R")


construct_input_tables(modname=FinModName,docname="annex\ plots/Annex_Tables")

# Summary data
FinMod <- readJJM(geth(FinModName,"h1"),path="config",input="input")
write.csv(summary(FinMod)$like,"results/SummaryLikelihoods_h1.csv")

FinMod <- readJJM(geth(FinModName,"h2"),path="config",input="input")
write.csv(summary(FinMod)$like,"results/SummaryLikelihoods_h2.csv")

#------------------
# Kobe for main doc
# Summary sheets
#------------------
kobe(FinMod)
kobe(fixed_bmsy(FinMod) ,add=T, col = 'red') 

main.diag <- diagnostics(fixed_bmsy(FinMod),plots=F)
plot(main.diag, var = "summarySheet")

main.diag <- diagnostics(FinMod,plots=F)
plot(main.diag, var = "summarySheet")






