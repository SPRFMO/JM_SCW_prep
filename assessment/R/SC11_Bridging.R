# ------------------------------------------------------------------------
# Script for SC11---------------------------------------------------------
# 2023 JM  ---------------------------------------------------------------
# ------------------------------------------------------------------------
#install.packages("devtools")
#
# MAKE SURE TO UPDATE
# devtools::install_github("sprfmo/jjmr")
# Remember to recompile jjm as needed in the src folder

library(jjmR)
library(tidyverse)
library(readxl)

#--------------------------------------------------------
# Working directory should be in assessment folder of jjm
#--------------------------------------------------------
# setwd(file.path(getwd(), "assessment"))
pwd <- getwd()
if (!grepl(basename(pwd), "assessment", ignore.case = TRUE)) {
  stop(paste("Set working directory to jjm/assessment"))
}

geth <- function(mod,h=hyp) paste0(h,"_", mod) # Package? Or keep?

fn_bridge <- function(newmod, newmodname, h2mod=h2_ctl,ln.dat=line_dat, ln.modnm=line_modnm) {

  names(newmod) <- newmod[[1]]$control$modelName <- geth(newmodname,"h1")
  newmod[[1]]$control$dataFile <- h2mod[ln.dat] <- paste0(newmodname,".dat")

  h2mod[ln.dat] <- paste0(newmodname, ".dat")
  h2mod[ln.modnm] <- geth(newmodname, "h2")
  
  writeJJM(newmod,datPath="input",ctlPath="config")
  writeLines(h2mod, con=paste0("config/h2_",newmodname,".ctl"))
}

fn_update <- function(newmod, newmodname, h) {

  names(newmod) <- newmod[[1]]$control$modelName <- geth(newmodname,h)
  newmod[[1]]$control$dataFile <- paste0(newmodname,".dat")
  
  writeJJM(newmod,datPath="input",ctlPath="config")
}

#---------------------------
# Save previous year's model
#---------------------------
# mod_prev_h1 <- readJJM(geth("1.02","h1"), path = "config", input = "input")
# save(mod_prev_h1, file="results/mod_prev_h1.Rdat")
# fn_update(mod_prev_h1, "0.00", "h1")

# mod_prev_h2 <- readJJM(geth("1.02","h2"), path = "config", input = "input")
# save(mod_prev_h2, file="results/mod_prev_h2.Rdat")
# fn_update(mod_prev_h2, "0.00", "h2")

# Move all previous year's model files to trash (results) or to arc folder (config & input)

#-------------------------
# Read in some data for making "new" datafiles
#-------------------------
hyp <- "h1"


# mod0.00 <- runit(geth("0.00",h="h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
# mod0.00 <- runit(geth("0.00",h="h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")

# Read in last year's datafile
mod_prev <- readJJM(geth("0.00"), path = "config", input = "input")

h2_ctl <- readLines("config/h2_0.00.ctl")
line_dat <- grep("dataFile", h2_ctl) + 1
line_modnm <- grep("modelName", h2_ctl) + 1


yr_prev <- as.numeric(format(Sys.time(), "%Y"))-1
yr_curr <- as.numeric(format(Sys.time(), "%Y"))
mod_new <- mod_prev

CV_CPUE <- 0.2
CV_acousN <- .5

file_input <- "data/SC11_2023assessmentInputData.xlsx" # From https://southpacificrfmo.sharepoint.com/:x:/r/sites/SPRFMOSCJackMackerelWorkingGroup/Shared%20Documents/Data%20repository/SC10_2022assessmentInputData.xlsx?d=w373d989fad0b492f8c80c637cbe6b437&csf=1&web=1&e=BP0PUM
file_catch <- "data/SC11_catchByFleet.csv" # From https://southpacificrfmo.sharepoint.com/:x:/r/sites/SPRFMOSC10/Shared%20Documents/Jack%20Mackerel/01%20Meeting%20Documents/SC10-JM01_rev1%20Annex1%20CJM%20catch%20history%20data.xlsx?d=w99cc1dc8a7d24120bcaabef504f9e1c9&csf=1&web=1&e=i9w0bX
# names_input <- excel_sheets(file_input)

dat_catch <- read_csv(file_catch) %>%
              rename_with(~ gsub("Fleet_", "fishery", .x, fixed = TRUE)) %>%
              select(-row,-fisherytotal) %>%
              mutate_at(vars(-year), function(x)x/1e3)

dat_wtatage_f <- read_excel(file_input, sheet=2) %>%
                  pivot_longer(cols=contains("F"), names_to="fleet") %>%
                  pivot_wider(names_from=age, values_from=value) %>%
                  mutate(fleet=as.numeric(str_extract(fleet,"\\d"))) %>%
                  select(-`0`)

dat_lencomp_f <- read_excel(file_input, sheet=3) %>%
                  pivot_longer(contains("F", ignore.case=F), names_to="fleet") %>%
                  pivot_wider(names_from=forklength, values_from=value) %>%
                  mutate(fleet=as.numeric(str_extract(fleet,"\\d"))) %>%
                  mutate_if(is.numeric,replace_na,0) %>%
                  rowwise() %>%
                  mutate(`10`=sum(c_across(`1`:`10`)),
                          `50`=sum(c_across(`50`:`100`))) %>%
                  select(-c(`1`:`9`),-c(`51`:`100`))

dat_agecomp_f <- read_excel(file_input, sheet=1) %>%
                  pivot_longer(contains("F"), names_to="fleet") %>%
                  pivot_wider(names_from=age, values_from=value) %>%
                  mutate(fleet=as.numeric(str_extract(fleet,"\\d"))) %>%
                  mutate_if(is.numeric,replace_na,0) %>%
                  select(-`0`)

dat_cpue <- read_excel(file_input, sheet=4) %>%
              pivot_longer(contains("F"), names_to="fleet", values_to="index") %>%
              mutate(fleet=as.numeric(str_extract(fleet,"\\d")),
                      err=CV_CPUE * index)

dat_acous <- read_excel(file_input, sheet=5) %>%
                      pivot_longer(contains("F"), names_to="fleet") %>%
                      pivot_wider(names_from=type,values_from=value) %>%
                      mutate(total_biomass=agecomp*wtatage,
                              fleet=as.numeric(str_extract(fleet,"\\d"))) %>%
                      replace_na(list("agecomp"=0,"total_biomass"=0)) %>%
                      filter(age>0, fleet==1) # Change for SC12?

#-----------
# 0.01 Update last year's catch
#-----------
catch_prev <- dat_catch %>%
              filter(year==yr_prev) %>%
              select(-year) %>%
              unlist()

mod_new[[1]]$data$Fcaton[which(rownames(mod_new[[1]]$data$Fcaton)==yr_prev),] <- catch_prev

# fn_bridge(mod_new, "0.01")
# mod0.01 <- runit(geth("0.01","h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
# mod0.01 <- runit(geth("0.01","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")

mod_prev <- mod_new
#----------
# 0.02 Last year's fishery age and length comp
#----------
mod_new <- mod_prev

for(f in 1:mod_new[[1]]$data$Fnum) {

  if(mod_new[[1]]$data$FnumyearsA[f] > 0) {  
    rows2use <- which(rownames(mod_new[[1]]$data$Fagecomp[,,f])==yr_prev)
    dat2use <- dat_agecomp_f %>%
              filter(year==yr_prev, fleet==f) %>%
              select(-year,-fleet) %>%
              unlist()

    if(length(dat2use)>0){
        if(sum(is.na(mod_new[[1]]$data$Fagecomp[rows2use,,f]))>0){
                mod_new[[1]]$data$FnumyearsA[f] <- mod_prev[[1]]$data$FnumyearsA[f] + 1
                mod_new[[1]]$data$Fageyears[rows2use,f] <- yr_prev
          }
        
        mod_new[[1]]$data$Fagecomp[rows2use,,f] <- dat2use
        mod_new[[1]]$data$Fagesample[rows2use,f] <- mod_new[[1]]$data$Fagesample[(rows2use-1),f]
      }
    }

  if(mod_new[[1]]$data$FnumyearsL[f] > 0) {

    rows2use <- which(rownames(mod_new[[1]]$data$Flengthcomp[,,f])==yr_prev)
    dat2use <- dat_lencomp_f %>%
          filter(year==yr_prev, fleet==f) %>%
          select(-year,-fleet) %>%
          unlist()
    if(length(dat2use)>0){
        if(sum(is.na(mod_new[[1]]$data$Flengthcomp[rows2use,,f]))>0){
                mod_new[[1]]$data$FnumyearsL[f] <- mod_prev[[1]]$data$FnumyearsL[f] + 1
                mod_new[[1]]$data$Flengthyears[rows2use,f] <- yr_prev
          }    
        mod_new[[1]]$data$Flengthcomp[rows2use,,f] <- dat2use
        mod_new[[1]]$data$Flengthsample[rows2use,f] <- mod_new[[1]]$data$Flengthsample[(rows2use-1),f]
      }
    }
}

# fn_bridge(mod_new, "0.02")
# mod0.02 <- runit(geth("0.02",h="h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
# mod0.02 <- runit(geth("0.02",h="h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
mod_prev <- mod_new


#----------
# 0.03 Last year's fishery (and CPUE) wtatage
# wtatage for fleet 3 is different?
#----------
mod_new <- mod_prev
cpue_ind <- grep("CPUE",mod_new[[1]]$data$Inames)

for(f in 1:mod_new[[1]]$data$Fnum) {
  rows2use <- which(rownames(mod_new[[1]]$data$Fwtatage[,,f])==yr_prev)
  if(f!=3) {
    dat2use <- dat_wtatage_f %>%
                filter(year==yr_prev, fleet==f) %>%
                select(-year,-fleet) %>%
                unlist()
    mod_new[[1]]$data$Fwtatage[rows2use,,f] <- dat2use
    mod_new[[1]]$data$Fwtatage[,,f] <- mod_new[[1]]$data$Fwtatage[,,f] %>% 
                                        as.data.frame() %>% 
                                        fill(everything()) %>% 
                                        as.matrix()
  }

 # Update wtatage for CPUE
  if(f>1) {
    rows2use <- which(rownames(mod_new[[1]]$data$Iwtatage[,,cpue_ind[f-1]])==yr_prev)
    mod_new[[1]]$data$Iwtatage[rows2use,,cpue_ind[f-1]] <- mod_new[[1]]$data$Fwtatage[rows2use,,f]
  }
}

# fn_bridge(mod_new, "0.03")
# mod0.03 <- runit(geth("0.03"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
# mod0.03 <- runit(geth("0.03","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
mod_prev <- mod_new

#----------
# 0.04 Previous year's offshore CPUE
#----------
mod_new <- mod_prev

i <- grep("Offshore",mod_new[[1]]$data$Inames)
dat2use <- dat_cpue %>% filter(fleet==4) %>% na.omit()
rows2use <- which(rownames(mod_new[[1]]$data$Index) %in% dat2use$year)

mod_new[[1]]$data$Inumyears[i] <- dim(dat2use)[1]
mod_new[[1]]$data$Index[,i] <- mod_new[[1]]$data$Indexerr[,i] <- mod_new[[1]]$data$Iyears[,i] <- NA # In case the years change... which they shouldn't.
mod_new[[1]]$data$Index[rows2use,i] <- dat2use$index
mod_new[[1]]$data$Indexerr[rows2use,i] <- dat2use$err
mod_new[[1]]$data$Iyears[rows2use,i] <- dat2use$year

# fn_bridge(mod_new, "0.04")
# mod0.04 <- runit(geth("0.04"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
# mod0.04 <- runit(geth("0.04","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
mod_prev <- mod_new


#----------
# 0.05 Add this year's catch projections
# Weight-at-age for current year filled with fleet-level mean from last five years
#----------
mod_new <- mod_prev

# Add year
mod_new[[1]]$data$years[2] <- yr_curr

# Add catch and catch error
catch_curr <- dat_catch %>%
              filter(year==yr_curr) %>%
              select(-year) %>%
              unlist()
mod_new[[1]]$data$Fcaton <- rbind(mod_prev[[1]]$data$Fcaton,catch_curr)
mod_new[[1]]$data$Fcatonerr <- rbind(mod_prev[[1]]$data$Fcatonerr, tail(mod_prev[[1]]$data$Fcatonerr,1))

mod_new[[1]]$data$Fwtatage <- array(dim=c(dim(mod_prev[[1]]$data$Fwtatage)[1]+1,dim(mod_prev[[1]]$data$Fwtatage)[2:3]))
# Add rows for wtatage for fishery and indices
for(f in 1:mod_new[[1]]$data$Fnum) {
  mod_new[[1]]$data$Fwtatage[,,f] <- rbind(mod_prev[[1]]$data$Fwtatage[,,f],tail(mod_prev[[1]]$data$Fwtatage[,,f],5) %>% colMeans())
}

mod_new[[1]]$data$Iwtatage <- array(dim=c(dim(mod_prev[[1]]$data$Iwtatage)[1]+1,dim(mod_prev[[1]]$data$Iwtatage)[2:3]))
for(i in 1:mod_new[[1]]$data$Inum) {
    mod_new[[1]]$data$Iwtatage[,,i] <- rbind(mod_prev[[1]]$data$Iwtatage[,,i],tail(mod_prev[[1]]$data$Iwtatage[,,i],5) %>% colMeans())
}

# fn_bridge(mod_new, "0.05")
# mod0.05 <- runit(geth("0.05"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
# mod0.05 <- runit(geth("0.05","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
mod_prev <- mod_new

#----------
# 0.06 Add current year's fishery length and age comps
#----------
mod_new <- readJJM("h1_0.05", path = "config", input = "input")
mod_prev <- mod_new

for(f in 1:mod_new[[1]]$data$Fnum) {

  if(mod_new[[1]]$data$FnumyearsA[f] > 0) {  
    rows2use <- which(rownames(mod_new[[1]]$data$Fagecomp[,,f])==yr_curr)
    dat2use <- dat_agecomp_f %>%
              filter(year==yr_curr, fleet==f) %>%
              select(-year,-fleet) %>%
              unlist()
    if(length(dat2use)>0){
        if(sum(is.na(mod_new[[1]]$data$Fagecomp[rows2use,,f]))>0){
                mod_new[[1]]$data$FnumyearsA[f] <- mod_prev[[1]]$data$FnumyearsA[f] + 1
                mod_new[[1]]$data$Fageyears[rows2use,f] <- yr_curr
          }
        
        mod_new[[1]]$data$Fagecomp[rows2use,,f] <- dat2use
        mod_new[[1]]$data$Fagesample[rows2use,f] <- mod_new[[1]]$data$Fagesample[(rows2use-1),f] * .1
      }
  }

  if(mod_new[[1]]$data$FnumyearsL[f] > 0) {
    rows2use <- which(rownames(mod_new[[1]]$data$Flengthcomp[,,f])==yr_curr)
    dat2use <- dat_lencomp_f %>%
          filter(year==yr_curr, fleet==f) %>%
          select(-year,-fleet) %>%
          unlist()
    if(length(dat2use)>0){
      if(sum(is.na(mod_new[[1]]$data$Flengthcomp[rows2use,,f]))>0){
            mod_new[[1]]$data$FnumyearsL[f] <- mod_prev[[1]]$data$FnumyearsL[f] + 1
            mod_new[[1]]$data$Flengthyears[rows2use,f] <- yr_curr
      }
        mod_new[[1]]$data$Flengthcomp[rows2use,,f] <- dat2use
        mod_new[[1]]$data$Flengthsample[rows2use,f] <- mod_new[[1]]$data$Flengthsample[(rows2use-1),f]
    }  
  }
}

# fn_bridge(mod_new, "0.06")
# mod0.06 <- runit("h1_0.06",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
# mod0.06 <- runit("h2_0.06",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
mod_prev <- mod_new


#----------
# 0.07 Current year's fishery (and CPUE) wtatage
#----------
mod_new <- mod_prev
cpue_ind <- grep("CPUE",mod_new[[1]]$data$Inames)

for(f in 1:mod_new[[1]]$data$Fnum) {
  rows2use <- which(rownames(mod_new[[1]]$data$Fwtatage[,,f])==yr_curr)
  if(f!=3){
    dat2use <- dat_wtatage_f %>%
                filter(year==yr_curr, fleet==f) %>%
                select(-year,-fleet) %>%
                unlist()
  
    if(length(dat2use)>0)
      mod_new[[1]]$data$Fwtatage[rows2use,,f] <- dat2use
      mod_new[[1]]$data$Fwtatage[,,f] <- mod_new[[1]]$data$Fwtatage[,,f] %>% 
                                        as.data.frame() %>% 
                                        fill(everything()) %>% 
                                        as.matrix()
  }

 # Update wtatage for CPUE
  if(f>1) {
    rows2use <- which(rownames(mod_new[[1]]$data$Iwtatage[,,cpue_ind[f-1]])==yr_curr)
    mod_new[[1]]$data$Iwtatage[rows2use,,cpue_ind[f-1]] <- mod_new[[1]]$data$Fwtatage[rows2use,,f]
  }
}

# fn_bridge(mod_new, "0.07")
# mod0.07 <- runit("h1_0.07",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
# mod0.07 <- runit("h2_0.07",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
mod_prev <- mod_new

#----------
# 0.08 SC Chile CPUE
#----------
mod_new <- mod_prev

i <- grep("Chile_CPUE",mod_new[[1]]$data$Inames)
dat2use <- dat_cpue %>% filter(fleet==2) %>% na.omit()
rows2use <- which(rownames(mod_new[[1]]$data$Index) %in% dat2use$year)

mod_new[[1]]$data$Inumyears[i] <- dim(dat2use)[1]
mod_new[[1]]$data$Index[,i] <- mod_new[[1]]$data$Indexerr[,i] <- mod_new[[1]]$data$Iyears[,i] <- NA # In case the years change... which they shouldn't.
mod_new[[1]]$data$Index[rows2use,i] <- dat2use$index
mod_new[[1]]$data$Indexerr[rows2use,i] <- dat2use$err
mod_new[[1]]$data$Iyears[rows2use,i] <- dat2use$year

# fn_bridge(mod_new, "0.08")
# mod0.08 <- runit("h1_0.08",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
# mod0.08 <- runit("h2_0.08",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
mod_prev <- mod_new


#----------
# 0.09 Peru CPUE
#----------
mod_new <- mod_prev

i <- grep("Peru_CPUE",mod_new[[1]]$data$Inames)
dat2use <- dat_cpue %>% filter(fleet==3) %>% na.omit()
rows2use <- which(rownames(mod_new[[1]]$data$Index) %in% dat2use$year)

mod_new[[1]]$data$Inumyears[i] <- dim(dat2use)[1]
mod_new[[1]]$data$Index[,i] <- mod_new[[1]]$data$Indexerr[,i] <- mod_new[[1]]$data$Iyears[,i] <- NA # In case the years change... which they shouldn't.
mod_new[[1]]$data$Index[rows2use,i] <- dat2use$index
mod_new[[1]]$data$Indexerr[rows2use,i] <- dat2use$err
mod_new[[1]]$data$Iyears[rows2use,i] <- dat2use$year

# fn_bridge(mod_new, "0.09")
# mod0.09 <- runit("h1_0.09",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
# mod0.09 <- runit("h2_0.09",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
mod_prev <- mod_new


#----------
# 0.10 Chile AcousN
#----------
mod_new <- mod_prev

i <- grep("Chile_AcousN",mod_new[[1]]$data$Inames)
rows2use <- which(rownames(mod_new[[1]]$data$Index) %in% unique(dat_acous$year))

# Update index
if(sum(is.na(mod_new[[1]]$data$Index[rows2use,i]))>0) {
  mod_new[[1]]$data$Inumyears[i] <- mod_prev[[1]]$data$Inumyears[i] + 1
  mod_new[[1]]$data$Iyears[rows2use,i] <- unique(dat_acous$year)
}

if(sum(is.na(mod_new[[1]]$data$Iyearsage[rows2use,i]))>0) {
  mod_new[[1]]$data$Iyearsage[rows2use,i] <- unique(dat_acous$year)
  mod_new[[1]]$data$Inumageyears[i] <- mod_prev[[1]]$data$Inumageyears[i] + 1
}

mod_new[[1]]$data$Index[rows2use,i] <- sum(dat_acous$total_biomass,na.rm=T)/1e3
mod_new[[1]]$data$Indexerr[rows2use,i] <- mod_new[[1]]$data$Index[rows2use,i] * CV_acousN

# Fixing mistake from SC10 where wt at age for recent years were set to zero for older ages
# Set to previous year's data because that was previously done
for(y in 1:nrow(mod_new[[1]]$data$Iwtatage[,,i])) {
  if(length(which(mod_new[[1]]$data$Iwtatage[y,,i]==0))>0){
      cols2change <- which(mod_new[[1]]$data$Iwtatage[y,,i]==0)
      mod_new[[1]]$data$Iwtatage[y,cols2change,i] <- mod_new[[1]]$data$Iwtatage[(y-1),cols2change,i]
    }
}

# Update wt at age
rows2use <- which(rownames(mod_new[[1]]$data$Iwtatage[,,i]) %in% unique(dat_acous$year))
mod_new[[1]]$data$Iwtatage[rows2use,,i] <- tail(mod_new[[1]]$data$Iwtatage[,,i],6)[-6,] %>% colMeans()
mod_new[[1]]$data$Iwtatage[rows2use,!is.na(dat_acous$wtatage),i] <- dat_acous$wtatage[!is.na(dat_acous$wtatage)]

# Update agecomp
rows2use <- which(rownames(mod_new[[1]]$data$Ipropage[,,i]) %in% unique(dat_acous$year))

mod_new[[1]]$data$Ipropage[rows2use,,i] <- dat_acous$agecomp
mod_new[[1]]$data$Iagesample[rows2use,i] <- mod_new[[1]]$data$Iagesample[rev(which(!is.na(mod_new[[1]]$data$Iagesample[,i])))[1],i]


# fn_bridge(mod_new, "0.10")
# mod0.10 <- runit(geth("0.10"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
# mod0.10 <- runit(geth("0.10","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
mod_prev <- mod_new


#-------------
# 1.00 Update CTL
#-------------
mod_h1 <- readJJM(geth("0.10","h1"), path = "config", input = "input")
mod_h2 <- readJJM(geth("0.10","h2"), path = "config", input = "input")

mod_h1_new <- mod_h1
mod_h2_new <- mod_h2

mod_h1_new[[1]]$control[["Nyrs_sr"]] <- mod_h1[[1]]$control[["Nyrs_sr"]]+1
mod_h1_new[[1]]$control[["Nyrs_sr_1"]] <- c(mod_h1[[1]]$control[["Nyrs_sr_1"]],(tail(mod_h1[[1]]$control[["Nyrs_sr_1"]],1)+1))

for(f in 1:mod_h1[[1]]$data$Fnum) {
  ff <- paste0("F",f,"_")

  mod_h1_new[[1]]$control[[paste0(ff,"info")]][length(mod_h1[[1]]$control[[paste0(ff,"info")]])] <- tail(mod_h1[[1]]$control[[paste0(ff,"info")]],1) + 1
  mod_h1_new[[1]]$control[[paste0(ff,"selchangeYear")]] <- c(mod_h1[[1]]$control[[paste0(ff,"selchangeYear")]],(tail(mod_h1[[1]]$control[[paste0(ff,"selchangeYear")]],1)+1))
  mod_h1_new[[1]]$control[[paste0(ff,"selchange")]] <- c(mod_h1[[1]]$control[[paste0(ff,"selchange")]],tail(mod_h1[[1]]$control[[paste0(ff,"selchange")]],1))
}


mod_h2_new[[1]]$control[["Nyrs_sr"]][1] <- mod_h2[[1]]$control[["Nyrs_sr"]][1]+1
mod_h2_new[[1]]$control[["Nyrs_sr_1"]] <- c(mod_h2[[1]]$control[["Nyrs_sr_1"]],(tail(mod_h2[[1]]$control[["Nyrs_sr_1"]],1)+1))

for(f in 1:mod_h2[[1]]$data$Fnum) {
  if(f!=3) {
    ff <- paste0("F",f,"_")
  
    mod_h2_new[[1]]$control[[paste0(ff,"info")]][length(mod_h2[[1]]$control[[paste0(ff,"info")]])] <- tail(mod_h2[[1]]$control[[paste0(ff,"info")]],1) + 1
    mod_h2_new[[1]]$control[[paste0(ff,"selchangeYear")]] <- c(mod_h2[[1]]$control[[paste0(ff,"selchangeYear")]],(tail(mod_h2[[1]]$control[[paste0(ff,"selchangeYear")]],1)+1))
    mod_h2_new[[1]]$control[[paste0(ff,"selchange")]] <- c(mod_h2[[1]]$control[[paste0(ff,"selchange")]],tail(mod_h2[[1]]$control[[paste0(ff,"selchange")]],1))
  }
}

# Change curve penalty back to 0.5 for final year
mod_h1_new[[1]]$control$F4_selchange[length(mod_h1_new[[1]]$control$F4_selchange)] <- mod_h2_new[[1]]$control$F4_selchange[length(mod_h2_new[[1]]$control$F4_selchange)]  <- 0.5

fn_update(mod_h1_new, "1.00", "h1")
fn_update(mod_h2_new, "1.00", "h2")

# mod1.00 <- runit(geth("1.00","h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
# mod1.00 <- runit(geth("1.00","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
