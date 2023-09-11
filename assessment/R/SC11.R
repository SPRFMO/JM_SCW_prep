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

file_dat <- h1_1.00[[1]]$data

# FinModName <- "1.02"

#----------
# Updating Chile_AcousCS
#----------

h1_prev <- h1_new <- h1_1.00
h1_new[[1]]$data <- file_dat
h1_prev <- h2_new <- h2_1.00
h2_new[[1]]$data <- file_dat

library(tidyverse)
library(readxl)

CV_acousCS <- 0.3

dat_acous_2020 <- "data/Age-data_Chile_New-criteria\ 1980-2022_august_9.xlsx" %>% # From I. Paya 10/09/23
                    read_excel(sheet=2) %>%
                    janitor::clean_names() %>%
                    filter(ano==2020, macrozona==2) %>%
                    select(ano,macrozona,grupo_edad,capt,p_pr,capt_ton) %>%
                    rename(year="ano", fleet="macrozona",age="grupo_edad",agecomp="capt",wtatage="p_pr",total_biomass="capt_ton")

dat_acous_2021 <- "data/SC10_2022assessmentInputData.xlsx" %>%
                      read_excel(sheet=5) %>%
                      pivot_longer(contains("F"), names_to="fleet") %>%
                      pivot_wider(names_from=type,values_from=value) %>%
                      mutate(total_biomass=agecomp*wtatage,
                              fleet=as.numeric(str_extract(fleet,"\\d"))) %>%
                      filter(age>0, fleet==2)

dat_acous_2023 <- "data/SC11_2023assessmentInputData.xlsx" %>%
                      read_excel(sheet=5) %>%
                      pivot_longer(contains("F"), names_to="fleet") %>%
                      pivot_wider(names_from=type,values_from=value) %>%
                      mutate(total_biomass=agecomp*wtatage,
                              fleet=as.numeric(str_extract(fleet,"\\d"))) %>%
                      filter(age>0, fleet==2)

dat_acous <- dat_acous_2023 %>%
              full_join(dat_acous_2021) %>%
              full_join(dat_acous_2020) %>%
              complete(year,age,fleet) %>%
              replace_na(list("agecomp"=0,"total_biomass"=0))

i <- grep("Chile_AcousCS",h1_new[[1]]$data$Inames)
rows2use <- which(rownames(h1_new[[1]]$data$Index) %in% unique(dat_acous$year))

# Update index
if(sum(is.na(h1_new[[1]]$data$Index[rows2use,i]))>0) {
  h1_new[[1]]$data$Inumyears[i] <- h1_prev[[1]]$data$Inumyears[i] + sum(is.na(h1_new[[1]]$data$Index[rows2use,i]))
  h1_new[[1]]$data$Iyears[rows2use,i] <- unique(dat_acous$year)
}

if(sum(is.na(h1_new[[1]]$data$Iyearsage[rows2use,i]))>0) {
  h1_new[[1]]$data$Iyearsage[rows2use,i] <- unique(dat_acous$year)
  h1_new[[1]]$data$Inumageyears[i] <- h1_prev[[1]]$data$Inumageyears[i] + sum(is.na(h1_new[[1]]$data$Iyearsage[rows2use,i]))
}

for(n in seq_along(rows2use)) {
  dat2use <- filter(dat_acous, year==unique(dat_acous$year)[n])
  h1_new[[1]]$data$Index[rows2use[n],i] <- sum(dat2use$total_biomass,na.rm=T)/1e3
  h1_new[[1]]$data$Indexerr[rows2use[n],i] <- h1_new[[1]]$data$Index[rows2use[n],i] * CV_acousCS
}

# Update wt at age
rows2use <- which(rownames(h1_new[[1]]$data$Iwtatage[,,i]) %in% unique(dat_acous$year))

for(n in seq_along(rows2use)) {
  dat2use <- filter(dat_acous, year==unique(dat_acous$year)[n])
  h1_new[[1]]$data$Iwtatage[rows2use[n],!is.na(dat2use$wtatage),i] <- dat2use$wtatage[!is.na(dat2use$wtatage)]
}

# Update agecomp
rows2use <- which(rownames(h1_new[[1]]$data$Ipropage[,,i]) %in% unique(dat_acous$year))
 
h1_new[[1]]$data$Ipropage[rows2use,,i] <- dat_acous$agecomp
h1_new[[1]]$data$Iagesample[rows2use,i] <- h1_new[[1]]$data$Iagesample[rev(which(!is.na(h1_new[[1]]$data$Iagesample[,i])))[1],i]


# fn_update(h1_new, "1.01", "h1", dodat=F)
# fn_update(h2_new, "1.01", "h2", dodat=F)
# h1_1.01 <- runit("h1_1.01",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
# h2_1.01 <- runit("h2_1.01",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")

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






