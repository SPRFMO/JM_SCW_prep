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
yr_curr <- as.numeric(format(Sys.time(), "%Y"))
mod_new <- mod_prev

CV.CPUE <- 0.2

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

dat.lencomp.f <- read_csv(here("data","SC09_catchLengthByFleet.csv")) %>%
                  select(-1) %>%
                  mutate_all(replace_na,0) %>%
                  rowwise() %>%
                  mutate(fleet=str_extract(fleet,"\\d"), 
                          `10`=sum(c_across(`9`:`10`)),
                          `50`=sum(c_across(`50`:`65`))) %>%
                  select(-`9`,-c(`51`:`65`))

dat.agecomp.f <- read_csv(here("data","SC09_catchNumByFleet.csv")) %>%
                  select(-1,-`0`) %>%
                  mutate_all(replace_na,0) %>%
                  mutate(fleet=str_extract(fleet,"\\d"))

dat.agecomp.f.new <- read_csv(here("data","SC09_catchNumByFleet_newAgeMet.csv")) %>% 
                      select(-1,-`0`) %>%
                      mutate_all(replace_na,0) %>%
                      mutate(fleet=str_extract(fleet,"\\d"))

dat.cpue.chile <- read_csv(here("data","SC09_CPUE_Chile_Trip.csv")) %>%
                    mutate(err=CV.CPUE * Index)

dat.cpue.offshore <- read_csv(here("data","SC09_CPUE_Offshore.csv")) %>%
                      mutate(err=CV.CPUE * cpue)


#-----------
# 0.01 Update last year's catch
#-----------
catch_prev <- dat.catch %>%
              filter(year==yr_prev) %>%
              select(-year) %>%
              unlist()

mod_new[[1]]$data$Fcaton[which(rownames(mod_new[[1]]$data$Fcaton)==yr_prev),] <- catch_prev

# fn.update(mod_new, "0.01")
# mod0.01 <- runit(geth("0.01"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

mod_prev <- mod_new
#----------
# 0.02 Last year's fishery age and length comp
# What to do with fleet 3 age comps
# Length comps for all the other fleets now?
# And length bins going out to 64 (previously only to 50). Sum up to plus group?
#----------
mod_new <- mod_prev

for(f in 1:mod_new[[1]]$data$Fnum) {

  if(mod_new[[1]]$data$FnumyearsA[f] > 0) {  
    row2use <- which(rownames(mod_new[[1]]$data$Fagecomp[,,f])==yr_prev)
    dat2use <- dat.agecomp.f %>%
              filter(year==yr_prev, fleet==f) %>%
              select(-year,-fleet) %>%
              unlist()

    if(length(dat2use)>0){
        if(sum(is.na(mod_new[[1]]$data$Fagecomp[row2use,,f]))>0){
                mod_new[[1]]$data$FnumyearsA[f] <- mod_prev[[1]]$data$FnumyearsA[f] + 1
                mod_new[[1]]$data$Fageyears[row2use,f] <- yr_prev
          }
        
        mod_new[[1]]$data$Fagecomp[row2use,,f] <- dat2use
        mod_new[[1]]$data$Fagesample[row2use,f] <- mod_new[[1]]$data$Fagesample[(row2use-1),f]
      }
    }

  if(mod_new[[1]]$data$FnumyearsL[f] > 0) {

    row2use <- which(rownames(mod_new[[1]]$data$Flengthcomp[,,f])==yr_prev)
    dat2use <- dat.lencomp.f %>%
          filter(year==yr_prev, fleet==f) %>%
          select(-year,-fleet) %>%
          unlist()
    if(length(dat2use)>0){
        if(sum(is.na(mod_new[[1]]$data$Flengthcomp[row2use,,f]))>0){
                mod_new[[1]]$data$FnumyearsL[f] <- mod_prev[[1]]$data$FnumyearsL[f] + 1
                mod_new[[1]]$data$Flengthyears[row2use,f] <- yr_prev
          }    
        mod_new[[1]]$data$Flengthcomp[row2use,,f] <- dat2use
        mod_new[[1]]$data$Flengthsample[row2use,f] <- mod_new[[1]]$data$Flengthsample[(row2use-1),f]
      }
    }
}

# fn.update(mod_new, "0.02")
# mod0.02 <- runit(geth("0.02"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod_prev <- mod_new


#----------
# 0.03 Last year's fishery (and CPUE) wtatage
# wtatage for fleet 3 is different?
#----------
mod_new <- mod_prev
cpue_ind <- grep("CPUE",mod_new[[1]]$data$Inames)

for(f in 1:mod_new[[1]]$data$Fnum) {
  row2use <- which(rownames(mod_new[[1]]$data$Fwtatage[,,f])==yr_prev)
  dat2use <- dat.wtatage.f %>%
              filter(year==yr_prev, fleet==f) %>%
              select(-year,-fleet) %>%
              unlist()
  mod_new[[1]]$data$Fwtatage[row2use,,f] <- dat2use
  mod_new[[1]]$data$Fwtatage[,,f] <- mod_new[[1]]$data$Fwtatage[,,f] %>% 
                                      as.data.frame() %>% 
                                      fill(everything()) %>% 
                                      as.matrix()

 # Update wtatage for CPUE
  if(f>1) {
    row2use <- which(rownames(mod_new[[1]]$data$Iwtatage[,,cpue_ind[f-1]])==yr_prev)
    mod_new[[1]]$data$Iwtatage[row2use,,cpue_ind[f-1]] <- mod_new[[1]]$data$Fwtatage[row2use,,f]
  }
}

# fn.update(mod_new, "0.03")
# mod0.03 <- runit(geth("0.03"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod_prev <- mod_new

#----------
# 0.04 Last year's offshore CPUE
# Got from SC9-JM01
#----------
mod_new <- mod_prev

i <- grep("Offshore",mod_new[[1]]$data$Inames)
rows2use <- which(rownames(mod_new[[1]]$data$Index) %in% dat.cpue.offshore$year)

if(sum(is.na(mod_new[[1]]$data$Index[rows2use,i]))>0) {
  mod_new[[1]]$data$Inumyears[i] <- mod_prev[[1]]$data$Inumyears[i] + sum(is.na(mod_prev[[1]]$data$Index[rows2use,i]))
}

mod_new[[1]]$data$Index[rows2use,i] <- dat.cpue.offshore$cpue
mod_new[[1]]$data$Indexerr[rows2use,i] <- dat.cpue.offshore$err
mod_new[[1]]$data$Iyears[rows2use,i] <- dat.cpue.offshore$year

# fn.update(mod_new, "0.04")
# mod0.04 <- runit(geth("0.04"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod_prev <- mod_new


#----------
# 0.05 Add 2021 catch projections
#----------
mod_new <- mod_prev

# Add year
mod_new[[1]]$data$years[2] <- yr_curr

# Add catch and catch error
catch_curr <- dat.catch %>%
              filter(year==yr_curr) %>%
              select(-year) %>%
              unlist()
mod_new[[1]]$data$Fcaton <- rbind(mod_prev[[1]]$data$Fcaton,catch_curr)
mod_new[[1]]$data$Fcatonerr <- rbind(mod_prev[[1]]$data$Fcatonerr, tail(mod_prev[[1]]$data$Fcatonerr,1))

mod_new[[1]]$data$Fwtatage <- array(dim=c(dim(mod_prev[[1]]$data$Fwtatage)[1]+1,dim(mod_prev[[1]]$data$Fwtatage)[2:3]))
# Add rows for wtatage for fishery and indices
for(f in 1:mod_new[[1]]$data$Fnum) {
  mod_new[[1]]$data$Fwtatage[,,f] <- rbind(mod_prev[[1]]$data$Fwtatage[,,f],tail(mod_prev[[1]]$data$Fwtatage[,,f],1))
}

mod_new[[1]]$data$Iwtatage <- array(dim=c(dim(mod_prev[[1]]$data$Iwtatage)[1]+1,dim(mod_prev[[1]]$data$Iwtatage)[2:3]))
for(i in 1:mod_new[[1]]$data$Inum) {
    mod_new[[1]]$data$Iwtatage[,,i] <- rbind(mod_prev[[1]]$data$Iwtatage[,,i],tail(mod_prev[[1]]$data$Iwtatage[,,i],1))
}

# fn.update(mod_new, "0.05")
# mod0.05 <- runit(geth("0.05"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod_prev <- mod_new

#----------
# 0.06 Add current year's length and age comps
#----------
mod_new <- readJJM(geth("0.05"), path = "config", input = "input")

for(f in 1:mod_new[[1]]$data$Fnum) {

  if(mod_new[[1]]$data$FnumyearsA[f] > 0) {  
    row2use <- which(rownames(mod_new[[1]]$data$Fagecomp[,,f])==yr_curr)
    dat2use <- dat.agecomp.f %>%
              filter(year==yr_curr, fleet==f) %>%
              select(-year,-fleet) %>%
              unlist()
    if(length(dat2use)>0){
        if(sum(is.na(mod_new[[1]]$data$Fagecomp[row2use,,f]))>0){
                mod_new[[1]]$data$FnumyearsA[f] <- mod_prev[[1]]$data$FnumyearsA[f] + 1
                mod_new[[1]]$data$Fageyears[row2use,f] <- yr_curr
          }
        
        mod_new[[1]]$data$Fagecomp[row2use,,f] <- dat2use
        mod_new[[1]]$data$Fagesample[row2use,f] <- mod_new[[1]]$data$Fagesample[(row2use-1),f] * .1
      }
  }

  if(mod_new[[1]]$data$FnumyearsL[f] > 0) {
    row2use <- which(rownames(mod_new[[1]]$data$Flengthcomp[,,f])==yr_curr)
    dat2use <- dat.lencomp.f %>%
          filter(year==yr_curr, fleet==f) %>%
          select(-year,-fleet) %>%
          unlist()
    if(length(dat2use)>0){
      if(sum(is.na(mod_new[[1]]$data$Flengthcomp[row2use,,f]))>0){
            mod_new[[1]]$data$FnumyearsL[f] <- mod_prev[[1]]$data$FnumyearsL[f] + 1
            mod_new[[1]]$data$Flengthyears[row2use,f] <- yr_curr
      }
        mod_new[[1]]$data$Flengthcomp[row2use,,f] <- dat2use
        mod_new[[1]]$data$Flengthsample[row2use,f] <- mod_new[[1]]$data$Flengthsample[(row2use-1),f]
    }  
  }
}

# fn.update(mod_new, "0.06")
# mod0.06 <- runit(geth("0.06"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod_prev <- mod_new


#----------
# 0.07 Current year's fishery (and CPUE) wtatage
# wtatage for fleet 3 is different?
#----------
mod_new <- mod_prev
cpue_ind <- grep("CPUE",mod_new[[1]]$data$Inames)

for(f in 1:mod_new[[1]]$data$Fnum) {
  row2use <- which(rownames(mod_new[[1]]$data$Fwtatage[,,f])==yr_curr)
  dat2use <- dat.wtatage.f %>%
              filter(year==yr_curr, fleet==f) %>%
              select(-year,-fleet) %>%
              unlist()

  if(length(dat2use)>0)
    mod_new[[1]]$data$Fwtatage[row2use,,f] <- dat2use
  mod_new[[1]]$data$Fwtatage[,,f] <- mod_new[[1]]$data$Fwtatage[,,f] %>% 
                                      as.data.frame() %>% 
                                      fill(everything()) %>% 
                                      as.matrix()

 # Update wtatage for CPUE
  if(f>1) {
    row2use <- which(rownames(mod_new[[1]]$data$Iwtatage[,,cpue_ind[f-1]])==yr_curr)
    mod_new[[1]]$data$Iwtatage[row2use,,cpue_ind[f-1]] <- mod_new[[1]]$data$Fwtatage[row2use,,f]
  }
}

# fn.update(mod_new, "0.07")
# mod0.07 <- runit(geth("0.07"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod_prev <- mod_new

#----------
# 0.08 SC Chile CPUE
# Got from Ignacio email 17/09/21
#----------
mod_new <- mod_prev

i <- grep("Chile_CPUE",mod_new[[1]]$data$Inames)
rows2use <- which(rownames(mod_new[[1]]$data$Index) %in% dat.cpue.chile$year)

if(sum(is.na(mod_new[[1]]$data$Index[rows2use,i]))>0) {
  mod_new[[1]]$data$Inumyears[i] <- mod_prev[[1]]$data$Inumyears[i] + sum(is.na(mod_prev[[1]]$data$Index[rows2use,i]))
}

mod_new[[1]]$data$Index[rows2use,i] <- dat.cpue.chile$Index
mod_new[[1]]$data$Indexerr[rows2use,i] <- dat.cpue.chile$err
mod_new[[1]]$data$Iyears[rows2use,i] <- dat.cpue.chile$year

# fn.update(mod_new, "0.08")
# mod0.08 <- runit(geth("0.08"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod_prev <- mod_new
