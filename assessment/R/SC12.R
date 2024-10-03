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
fn_bridge <- function(newmod, newmodname, h2mod,ln_dat=line_dat, ln_modnm=line_modnm) {

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
cv_cpue <- 0.2
y <- which(rownames(h1_1.01[[1]]$data$Index) == 2022)
h1_1.01[[1]]$data$Indexerr[y,i] <- h2_1.01[[1]]$data$Indexerr[y,i] <- h1_1.01[[1]]$data$Index[y,i] * cv_cpue

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
                rowid_to_column() %>%
                mutate(creep = 0.99^(rowid-1)) %>%
                mutate(cpue_creep = mean * creep,
                        low_creep = low * creep,
                        sd = mean - low)

cv_cpue <- 0.2

i <- grep("Chile_CPUE",h1_1.02[[1]]$data$Inames)
rows2use <- which(rownames(h1_1.02[[1]]$data$Index) %in% chile_cpue$year)

h1_1.02[[1]]$data$Inumyears[i]          <- dim(chile_cpue)[1]
h1_1.02[[1]]$data$Index[,i]             <- h1_1.02[[1]]$data$Indexerr[,i] <- h1_1.02[[1]]$data$Iyears[,i] <- NA # In case the years change.
h1_1.02[[1]]$data$Index[rows2use,i]     <- chile_cpue$cpue_creep
h1_1.02[[1]]$data$Indexerr[rows2use,i]  <- chile_cpue$cpue_creep * cv_cpue
h1_1.02[[1]]$data$Iyears[rows2use,i]    <- chile_cpue$year

fn_bridge(h1_1.02, "1.02", h2mod=h2_ctl)
mod_1.02 <- runit(geth("1.02",c("h1","h2")),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm", parallel=TRUE, adflags=paste0("-tac ", tac_prev))


#----------
# New CPUE index proposed in JM07
# Using regular CPUE CV of 0.2
#----------
h1_1.03 <- h1_1.02

h1_1.03[[1]]$data$Indexerr[rows2use,i]    <- chile_cpue$sd

fn_bridge(h1_1.03, "1.03", h2mod=h2_ctl)
mod_1.03 <- runit(geth("1.03",c("h1","h2")),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm", parallel=TRUE, adflags=paste0("-tac ", tac_prev))


#---------
# Updating selectivity change for N_Chile
#---------
h1_1.01 <- readJJM(geth("1.01","h1"), path = "config", input = "input")
h2_1.01 <- readJJM(geth("1.01","h2"), path = "config", input = "input")

h1_1.04 <- h1_1.01
h2_1.04 <- h2_1.01

ff <- "F1_"

h1_1.04[[1]]$control[[paste0(ff,"info")]][length(h1_1.04[[1]]$control[[paste0(ff,"info")]])] <- tail(h1_1.04[[1]]$control[[paste0(ff,"info")]],1) + 1
h1_1.04[[1]]$control[[paste0(ff,"selchangeYear")]] <- c(h1_1.04[[1]]$control[[paste0(ff,"selchangeYear")]],(tail(h1_1.04[[1]]$control[[paste0(ff,"selchangeYear")]],1)+1))
h1_1.04[[1]]$control[[paste0(ff,"selchange")]] <- c(h1_1.04[[1]]$control[[paste0(ff,"selchange")]],tail(h1_1.04[[1]]$control[[paste0(ff,"selchange")]],1))

h2_1.04[[1]]$control[[paste0(ff,"info")]][length(h2_1.04[[1]]$control[[paste0(ff,"info")]])] <- tail(h2_1.04[[1]]$control[[paste0(ff,"info")]],1) + 1
h2_1.04[[1]]$control[[paste0(ff,"selchangeYear")]] <- c(h2_1.04[[1]]$control[[paste0(ff,"selchangeYear")]],(tail(h2_1.04[[1]]$control[[paste0(ff,"selchangeYear")]],1)+1))
h2_1.04[[1]]$control[[paste0(ff,"selchange")]] <- c(h2_1.04[[1]]$control[[paste0(ff,"selchange")]],tail(h2_1.04[[1]]$control[[paste0(ff,"selchange")]],1))


fn_update(h1_1.04, "1.04", "h1")
fn_update(h2_1.04, "1.04", "h2")
mod_1.04 <- runit(geth("1.04",c("h1","h2")),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm", parallel=TRUE, adflags=paste0("-tac ", tac_prev))

#---------
# Correcting AcousN age composition data
#----------
h1_1.04 <- readJJM(geth("1.04","h1"), path = "config", input = "input")
h1_1.05 <- h1_1.04

library(readxl)
dat_acous <- read_excel(here::here("data", "Revised historic North Acoustic JM.xlsx")) %>%
              select(!contains("...")) %>%
              rename(age=AGE) %>%
              mutate(age=0:12) %>%
              pivot_longer(-age, names_to="year", values_to="agecomp") %>%
              group_by(year) %>%
              replace_na(list(agecomp=0)) %>%
              mutate(propage=agecomp/sum(agecomp),
                data="new") %>%
              ungroup() %>%
              filter(age!=0) %>%
              drop_na()

i <- grep("Chile_AcousN",h1_1.05[[1]]$data$Inames)
dat_prev <- h1_1.05[[1]]$data[["Ipropage"]][,,i] %>%
  as.data.frame() %>%
  rownames_to_column("year") %>%
  as_tibble() %>%
  pivot_longer(-year, names_to="age", values_to="agecomp") %>%
  drop_na() %>%
  mutate(data="current",
    age=as.numeric(age)) %>%
  group_by(year) %>%
  mutate(propage=agecomp/sum(agecomp)) %>%
  ungroup()

dat_acous %>%
  add_row(dat_prev) %>%
  ggplot() +
    geom_line(aes(x=age,y=propage,colour=data), alpha=.6) +
    facet_wrap(~year) +
    theme_jjm()


rows2use <- which(rownames(h1_1.05[[1]]$data$Index) %in% unique(dat_acous$year))
h1_1.05[[1]]$data$Ipropage[rows2use,,i] <- dat_acous$agecomp

fn_bridge(h1_1.05, "1.05",h2mod=readLines("config/h2_1.04.ctl"))
mod_1.05 <- runit(geth("1.05",c("h1","h2")),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm", parallel=TRUE, adflags=paste0("-tac ", tac_prev))

#---------
# Decreasing CV for AcousN
#----------
# h1_1.05 <- readJJM(geth("1.05","h1"), path = "config", input = "input")
h1_1.06 <- h1_1.05

cv_acousN <- 0.3

i <- grep("Chile_AcousN",h1_1.05[[1]]$data$Inames)
h1_1.06[[1]]$data$Indexerr[,i] <- h1_1.06[[1]]$data$Index[,i] * cv_acousN

fn_bridge(h1_1.06, "1.06", h2mod=readLines("config/h2_1.05.ctl"))
mod_1.06 <- runit(geth("1.06",c("h1","h2")),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm", parallel=TRUE, adflags=paste0("-tac ", tac_prev))


#---------
# Downweighting most current year CPUE
# Double the CV because it's only for first half of the year?
#----------
h1_1.06 <- readJJM(geth("1.06","h1"), path = "config", input = "input")
h1_1.07 <- h1_1.06

cv_cpue <- 0.2
cpue_ind <- grep("CPUE",h1_1.07[[1]]$data$Inames)
row2use <- dim(h1_1.07[[1]]$data$Iyears)[1]

for(i in cpue_ind) {
  if(!is.na(h1_1.07[[1]]$data$Indexerr[row2use,i]))
    h1_1.07[[1]]$data$Indexerr[row2use,i] <- h1_1.07[[1]]$data$Index[row2use,i] * cv_cpue * 2
}

fn_bridge(h1_1.07, "1.07",h2mod=readLines("config/h2_1.06.ctl"))
mod_1.07 <- runit(geth("1.07",c("h1","h2")),parallel=TRUE,pdf=TRUE,portrait=F,est=T,exec="../src/jjm")


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
