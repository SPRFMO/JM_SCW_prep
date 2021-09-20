# ------------------------------------------------------------------------
# Script for SC09---------------------------------------------------------
# 2021 JM  ---------------------------------------------------------------
# ------------------------------------------------------------------------
#install.packages("devtools")
#
# MAKE SURE TO UPDATE
# devtools::install_github("sprfmo/jjmr")
# Remember to recompile jjms as needed in the src folder

library(jjmR)
library(tidyverse)
library(here)

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
fixed_bmsy <- function(mod,refpt=5500){
  old_rat <- (mod[[1]]$output[[1]]$msy_mt[,13])
  new_rat <- (mod[[1]]$output[[1]]$msy_mt[,12]/ refpt)
  mod[[1]]$output[[1]]$msy_mt[,13] <- new_rat
  mod[[1]]$output[[1]]$msy_mt[,10] <- refpt
  return(mod)
}


fn.update <- function(newmod, newmodname, h=hyp) {

  names(newmod) <- newmod[[1]]$control$modelName <- geth(newmodname,h)
  newmod[[1]]$control$dataFile <- paste0(newmodname,".dat")
  writeJJM(newmod,datPath="input",ctlPath="config")
}


#-------------------------
# Read in some data for making "new" datafiles
#-------------------------
hyp <- "h1"
 # mod0.00 <- runit(geth("0.00",h="h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# Read in last year's datafile
mod_prev <- readJJM(geth("0.00"), path = "config", input = "input")
yr_prev <- as.numeric(format(Sys.time(), "%Y"))-1
mod_new <- mod_prev


dat.catch <- read_csv(here("data","SC09_catchByFleet.csv")) %>%
              rename_with(~ gsub("Fleet_", "fishery", .x, fixed = TRUE)) %>%
              select(-row,-fisherytotal) %>%
              mutate_at(vars(-year), function(x)x/1e3)

dat.wtatage.f <- read_csv(here("data","SC09_catchWtByFleet.csv")) %>%
                  select(-1,-`0`) %>%
                  mutate(fleet=str_extract(fleet,"\\d"))

dat.wtatage.f.new <- read_csv(here("data","SC09_catchWtByFleet_newAgeMet.csv")) %>%
                  select(-1,-`0`) %>%
                  mutate(fleet=str_extract(fleet,"\\d"))

dat.lencomp <- read_csv(here::here("data","SC09_catchLengthByFleet.csv"))

dat.agecomp.f <- read_csv(here::here("data","SC09_catchNumByFleet.csv")) %>%
                  select(-1,-`0`) %>%
                  mutate_all(replace_na,0) %>%
                  mutate(fleet=str_extract(fleet,"\\d"))

dat.agecomp.f.new <- read_csv(here::here("data","SC09_catchNumByFleet_newAgeMet.csv")) %>% 
                      select(-1,-`0`) %>%
                      mutate_all(replace_na,0) %>%
                      mutate(fleet=str_extract(fleet,"\\d"))

dat.chile.cpue <- read_csv(here::here("data","SC09_Chile_Trip_CPUE.csv"))

#-----------
# 0.01 Update last year's catch
#-----------
catch_prev <- dat.catch %>%
              filter(year==yr_prev) %>%
              select(-year) %>%
              unlist()

mod_new[[1]]$data$Fcaton[which(rownames(mod_new[[1]]$data$Fcaton)==yr_prev),] <- catch_prev

fn.update(mod_new, "0.01")
# mod0.01 <- runit(geth("0.01"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

mod_prev <- mod_new
#----------
# 0.02 Last year's fishery age comp, wtatage and CPUE
# What to do with fleet 3 age comps
# wtatage for fleet 3 is different?
#----------
mod_new <- mod_prev
cpue_ind <- grep("CPUE",mod_new[[1]]$data$Inames)

for(f in 1:dim(mod_new[[1]]$data$Fagecomp)[3]) {
  row2use <- which(rownames(mod_new[[1]]$data$Fagecomp[,,f])==yr_prev)
  dat2use <- dat.agecomp.f %>%
              filter(year==yr_prev, fleet==f) %>%
              select(-year,-fleet) %>%
              unlist()
  if(mod_new[[1]]$data$FnumyearsA[f] > 0) {
    if(sum(is.na(mod_new[[1]]$data$Fagecomp[row2use,,f]))>0){
            mod_new[[1]]$data$FnumyearsA[f] <- mod_new[[1]]$data$FnumyearsA[f] + 1
            mod_new[[1]]$data$Fageyears[row2use,f] <- yr_prev
      }
    
    mod_new[[1]]$data$Fagecomp[row2use,,f] <- dat2use
    mod_new[[1]]$data$Fagesample[row2use,f] <- mod_new[[1]]$data$Fagesample[(row2use-1),f]
  }
  dat2use <- dat.wtatage.f %>%
              filter(year==yr_prev, fleet==f) %>%
              select(-year,-fleet) %>%
              unlist()
  mod_new[[1]]$data$Fwtatage[row2use,,f] <- dat2use
  mod_new[[1]]$data$Fwtatage[,,f] <- mod_prev[[1]]$data$Fwtatage[,,f] %>% 
                                      as.data.frame() %>% 
                                      fill(everything()) %>% 
                                      as.matrix()

 # Update wtatage for CPUE
  if(f>1) {
    mod_new[[1]]$data$Iwtatage[row2use,,cpue_ind[f-1]] <- dat2use
    mod_new[[1]]$data$Iwtatage[,,cpue_ind[f-1]] <- mod_new[[1]]$data$Iwtatage[,,cpue_ind[f-1]] %>% 
                                      as.data.frame() %>% 
                                      fill(everything()) %>% 
                                      as.matrix()
  }
}

fn.update(mod_new, "0.02")
# mod0.02 <- runit(geth("0.02"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")


