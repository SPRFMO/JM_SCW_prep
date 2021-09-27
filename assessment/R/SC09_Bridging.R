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

fn.bridge <- function(newmod, newmodname, h2mod=h2.ctl,ln.dat=line.dat, ln.modnm=line.modnm) {

  names(newmod) <- newmod[[1]]$control$modelName <- geth(newmodname,"h1")
  newmod[[1]]$control$dataFile <- h2mod[line.dat] <- paste0(newmodname,".dat")

  h2mod[ln.dat] <- paste0(newmodname, ".dat")
  h2mod[ln.modnm] <- geth(newmodname, "h2")
  
  writeJJM(newmod,datPath="input",ctlPath="config")
  writeLines(h2mod, con=paste0("config/h2_",newmodname,".ctl"))
}

fn.update <- function(newmod, newmodname, h) {

  names(newmod) <- newmod[[1]]$control$modelName <- geth(newmodname,h)
  newmod[[1]]$control$dataFile <- paste0(newmodname,".dat")
  
  writeJJM(newmod,datPath="input",ctlPath="config")
}


#-------------------------
# Read in some data for making "new" datafiles
#-------------------------
hyp <- "h1"
 # mod0.00 <- runit(geth("0.00",h="h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.00 <- runit(geth("0.00",h="h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

# Read in last year's datafile
mod_prev <- readJJM(geth("0.00"), path = "config", input = "input")

h2.ctl <- readLines("config/h2_0.00.ctl")
line.dat <- grep("dataFile", h2.ctl) + 1
line.modnm <- grep("modelName", h2.ctl) + 1


yr_prev <- as.numeric(format(Sys.time(), "%Y"))-1
yr_curr <- as.numeric(format(Sys.time(), "%Y"))
mod_new <- mod_prev

CV.CPUE <- 0.2
CV.acousN <- .5

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

dat.cpue.peru <- read_csv(here("data","SC09_CPUE_Peru.csv")) %>%
                      mutate(err=CV.CPUE * cpue)

dat.acousN.ant <- read_csv(here("data","SC09_AcousN_Antiguo.csv")) %>%
                      janitor::clean_names() %>%
                      mutate(numbers_at_age=as.numeric(str_remove_all(replace_na(numbers_at_age,0)," ")),
                              total_biomass=replace_na(total_biomass,0)) %>%
                      filter(age>0)

dat.acousN.nue <- read_csv(here("data","SC09_AcousN_Nuevo.csv")) %>%
                      janitor::clean_names() %>%
                      mutate(numbers_at_age=as.numeric(str_remove_all(replace_na(numbers_at_age,0)," ")),
                              total_biomass=replace_na(total_biomass,0)) %>%
                      filter(age>0)

#-----------
# 0.01 Update last year's catch
#-----------
catch_prev <- dat.catch %>%
              filter(year==yr_prev) %>%
              select(-year) %>%
              unlist()

mod_new[[1]]$data$Fcaton[which(rownames(mod_new[[1]]$data$Fcaton)==yr_prev),] <- catch_prev

# fn.bridge(mod_new, "0.01")
# mod0.01 <- runit(geth("0.01","h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.01 <- runit(geth("0.01","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

mod_prev <- mod_new
#----------
# 0.02 Last year's fishery age and length comp
# What to do with fleet 3 age comps
# And length bins going out to 64 (previously only to 50). Sum up to plus group?
#----------
mod_new <- mod_prev

for(f in 1:mod_new[[1]]$data$Fnum) {

  if(mod_new[[1]]$data$FnumyearsA[f] > 0) {  
    rows2use <- which(rownames(mod_new[[1]]$data$Fagecomp[,,f])==yr_prev)
    dat2use <- dat.agecomp.f %>%
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
    dat2use <- dat.lencomp.f %>%
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

# fn.bridge(mod_new, "0.02")
# mod0.02 <- runit(geth("0.02",h="h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.02 <- runit(geth("0.02",h="h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod_prev <- mod_new


#----------
# 0.03 Last year's fishery (and CPUE) wtatage
# wtatage for fleet 3 is different?
#----------
mod_new <- mod_prev
cpue_ind <- grep("CPUE",mod_new[[1]]$data$Inames)

for(f in 1:mod_new[[1]]$data$Fnum) {
  rows2use <- which(rownames(mod_new[[1]]$data$Fwtatage[,,f])==yr_prev)
  dat2use <- dat.wtatage.f %>%
              filter(year==yr_prev, fleet==f) %>%
              select(-year,-fleet) %>%
              unlist()
  mod_new[[1]]$data$Fwtatage[rows2use,,f] <- dat2use
  mod_new[[1]]$data$Fwtatage[,,f] <- mod_new[[1]]$data$Fwtatage[,,f] %>% 
                                      as.data.frame() %>% 
                                      fill(everything()) %>% 
                                      as.matrix()

 # Update wtatage for CPUE
  if(f>1) {
    rows2use <- which(rownames(mod_new[[1]]$data$Iwtatage[,,cpue_ind[f-1]])==yr_prev)
    mod_new[[1]]$data$Iwtatage[rows2use,,cpue_ind[f-1]] <- mod_new[[1]]$data$Fwtatage[rows2use,,f]
  }
}

# fn.bridge(mod_new, "0.03")
# mod0.03 <- runit(geth("0.03"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.03 <- runit(geth("0.03","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
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

# fn.bridge(mod_new, "0.04")
# mod0.04 <- runit(geth("0.04"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.04 <- runit(geth("0.04","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod_prev <- mod_new


#----------
# 0.05 Add this year's catch projections
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

# fn.bridge(mod_new, "0.05")
# mod0.05 <- runit(geth("0.05"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.05 <- runit(geth("0.05","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod_prev <- mod_new

#----------
# 0.06 Add current year's length and age comps
#----------
mod_new <- readJJM(geth("0.05"), path = "config", input = "input")

for(f in 1:mod_new[[1]]$data$Fnum) {

  if(mod_new[[1]]$data$FnumyearsA[f] > 0) {  
    rows2use <- which(rownames(mod_new[[1]]$data$Fagecomp[,,f])==yr_curr)
    dat2use <- dat.agecomp.f %>%
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
    dat2use <- dat.lencomp.f %>%
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

# fn.bridge(mod_new, "0.06")
# mod0.06 <- runit(geth("0.06"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.06 <- runit(geth("0.06","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod_prev <- mod_new


#----------
# 0.07 Current year's fishery (and CPUE) wtatage
# wtatage for fleet 3 is different?
#----------
mod_new <- mod_prev
cpue_ind <- grep("CPUE",mod_new[[1]]$data$Inames)

for(f in 1:mod_new[[1]]$data$Fnum) {
  rows2use <- which(rownames(mod_new[[1]]$data$Fwtatage[,,f])==yr_curr)
  dat2use <- dat.wtatage.f %>%
              filter(year==yr_curr, fleet==f) %>%
              select(-year,-fleet) %>%
              unlist()

  if(length(dat2use)>0)
    mod_new[[1]]$data$Fwtatage[rows2use,,f] <- dat2use
    mod_new[[1]]$data$Fwtatage[,,f] <- mod_new[[1]]$data$Fwtatage[,,f] %>% 
                                      as.data.frame() %>% 
                                      fill(everything()) %>% 
                                      as.matrix()

 # Update wtatage for CPUE
  if(f>1) {
    rows2use <- which(rownames(mod_new[[1]]$data$Iwtatage[,,cpue_ind[f-1]])==yr_curr)
    mod_new[[1]]$data$Iwtatage[rows2use,,cpue_ind[f-1]] <- mod_new[[1]]$data$Fwtatage[rows2use,,f]
  }
}

# fn.bridge(mod_new, "0.07")
# mod0.07 <- runit(geth("0.07"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.07 <- runit(geth("0.07","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
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

# fn.bridge(mod_new, "0.08")
# mod0.08 <- runit(geth("0.08"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.08 <- runit(geth("0.08","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod_prev <- mod_new


#----------
# 0.09 Peru CPUE
#----------
mod_new <- mod_prev

i <- grep("Peru_CPUE",mod_new[[1]]$data$Inames)
rows2use <- which(rownames(mod_new[[1]]$data$Index) %in% dat.cpue.peru$year)

if(sum(is.na(mod_new[[1]]$data$Index[rows2use,i]))>0) {
  mod_new[[1]]$data$Inumyears[i] <- mod_prev[[1]]$data$Inumyears[i] + sum(is.na(mod_prev[[1]]$data$Index[rows2use,i]))
}

mod_new[[1]]$data$Index[rows2use,i] <- dat.cpue.peru$cpue
mod_new[[1]]$data$Indexerr[rows2use,i] <- dat.cpue.peru$err
mod_new[[1]]$data$Iyears[rows2use,i] <- dat.cpue.peru$year

# fn.bridge(mod_new, "0.09")
# mod0.09 <- runit(geth("0.09"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.09 <- runit(geth("0.09","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod_prev <- mod_new

#----------
# 0.10 Chile AcousN
#----------
mod_new <- mod_prev

i <- grep("Chile_AcousN",mod_new[[1]]$data$Inames)
rows2use <- which(rownames(mod_new[[1]]$data$Index) == yr_curr)

# Update index
if(sum(is.na(mod_new[[1]]$data$Index[rows2use,i]))>0) {
  mod_new[[1]]$data$Inumyears[i] <- mod_prev[[1]]$data$Inumyears[i] + 1
  mod_new[[1]]$data$Iyears[rows2use,i] <- yr_curr
}

if(sum(is.na(mod_new[[1]]$data$Iyearsage[rows2use,i]))>0) {
  mod_new[[1]]$data$Iyearsage[rows2use,i] <- yr_curr
  mod_new[[1]]$data$Inumageyears[i] <- mod_prev[[1]]$data$Inumageyears[i] + 1
}

mod_new[[1]]$data$Index[rows2use,i] <- sum(dat.acousN.ant$total_biomass,na.rm=T)/1e3
mod_new[[1]]$data$Indexerr[rows2use,i] <- mod_new[[1]]$data$Index[rows2use,i] * CV.acousN

# Update wt at age
rows2use <- which(rownames(mod_new[[1]]$data$Iwtatage[,,i])==yr_curr)
mod_new[[1]]$data$Iwtatage[rows2use,!is.na(dat.acousN.ant$mean_weight),i] <- dat.acousN.ant$mean_weight[!is.na(dat.acousN.ant$mean_weight)]

# Update agecomp
rows2use <- which(rownames(mod_new[[1]]$data$Ipropage[,,i])==yr_curr)

mod_new[[1]]$data$Ipropage[rows2use,,i] <- dat.acousN.ant$numbers_at_age
mod_new[[1]]$data$Iagesample[rows2use,i] <- mod_new[[1]]$data$Iagesample[(rows2use-1),i]

# fn.bridge(mod_new, "0.10")
# mod0.10 <- runit(geth("0.10"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.10 <- runit(geth("0.10","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod_prev <- mod_new

#-------------
# 1.00 Update CTL
#-------------
mod.h1 <- readJJM(geth("0.10","h1"), path = "config", input = "input")
mod.h2 <- readJJM(geth("0.10","h2"), path = "config", input = "input")

mod.h1.new <- mod.h1
mod.h2.new <- mod.h2

mod.h1.new[[1]]$control[["Nyrs_sr"]] <- mod.h1[[1]]$control[["Nyrs_sr"]]+1
mod.h1.new[[1]]$control[["Nyrs_sr_1"]] <- c(mod.h1[[1]]$control[["Nyrs_sr_1"]],(tail(mod.h1[[1]]$control[["Nyrs_sr_1"]],1)+1))

for(f in 1:mod.h1[[1]]$data$Fnum) {
  ff <- paste0("F",f,"_")

  mod.h1.new[[1]]$control[[paste0(ff,"info")]][length(mod.h1[[1]]$control[[paste0(ff,"info")]])] <- tail(mod.h1[[1]]$control[[paste0(ff,"info")]],1) + 1
  mod.h1.new[[1]]$control[[paste0(ff,"selchangeYear")]] <- c(mod.h1[[1]]$control[[paste0(ff,"selchangeYear")]],(tail(mod.h1[[1]]$control[[paste0(ff,"selchangeYear")]],1)+1))
  mod.h1.new[[1]]$control[[paste0(ff,"selchange")]] <- c(mod.h1[[1]]$control[[paste0(ff,"selchange")]],tail(mod.h1[[1]]$control[[paste0(ff,"selchange")]],1))
}


mod.h2.new[[1]]$control[["Nyrs_sr"]][1] <- mod.h2[[1]]$control[["Nyrs_sr"]][1]+1
mod.h2.new[[1]]$control[["Nyrs_sr_1"]] <- c(mod.h2[[1]]$control[["Nyrs_sr_1"]],(tail(mod.h2[[1]]$control[["Nyrs_sr_1"]],1)+1))

for(f in 1:mod.h2[[1]]$data$Fnum) {
  if(f!=3) {
    ff <- paste0("F",f,"_")
  
    mod.h2.new[[1]]$control[[paste0(ff,"info")]][length(mod.h2[[1]]$control[[paste0(ff,"info")]])] <- tail(mod.h2[[1]]$control[[paste0(ff,"info")]],1) + 1
    mod.h2.new[[1]]$control[[paste0(ff,"selchangeYear")]] <- c(mod.h2[[1]]$control[[paste0(ff,"selchangeYear")]],(tail(mod.h2[[1]]$control[[paste0(ff,"selchangeYear")]],1)+1))
    mod.h2.new[[1]]$control[[paste0(ff,"selchange")]] <- c(mod.h2[[1]]$control[[paste0(ff,"selchange")]],tail(mod.h2[[1]]$control[[paste0(ff,"selchange")]],1))
  }
}

# fn.update(mod.h1.new, "1.00", "h1")
# mod1.00 <- runit(geth("1.00","h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

# fn.update(mod.h2.new, "1.00", "h2")
# mod1.00 <- runit(geth("1.00","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
