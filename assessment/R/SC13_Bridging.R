# ------------------------------------------------------------------------
# Script for SC13---------------------------------------------------------
# 2025 JM  ---------------------------------------------------------------
# ------------------------------------------------------------------------
#install.packages("devtools")
#
# MAKE SURE TO UPDATE
# devtools::install_github("sprfmo/jjmr")
# Remember to recompile jjm as needed in the src folder

library(jjmR)
library(tidyverse)
library(readxl)
library(foreach)
library(doParallel)
ncores <- 2
cl <- if(Sys.info()[["sysname"]]=="Windows") makePSOCKcluster(ncores) else ncores
registerDoParallel(cl)

# Working directory should be in assessment folder of jjm

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

# If fail to converge, ./jjm -binp jjm.b02 -ind xx.ctl

#---------------------------
# Save previous year's model
#---------------------------
# finmodname <- "1.07"
# mod_prev_h1 <- readJJM(geth(finmodname,"h1"), path = "config", input = "input")
# mod_prev_h2 <- readJJM(geth(finmodname,"h2"), path = "config", input = "input")


# Move all previous year's model files to trash (results) or to arc folder (config & input)

# save(mod_prev_h1, file="results/mod_prev_h1.Rdat")
# save(mod_prev_h2, file="results/mod_prev_h2.Rdat")

# fn_update(mod_prev_h1, "0.00", "h1")

# fn_update(mod_prev_h2, "0.00", "h2")

#-------------------------
# Read in some data for making "new" datafiles
#-------------------------
hyp <- "h1"

# mod0.00 <- runit(geth("0.00",c("h1","h2")),pdf=F,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)

# Read in last year's datafile
mod_prev <- readJJM(geth("0.00"), path = "config", input = "input")

h2_ctl <- readLines("config/h2_0.00.ctl")
line_dat <- grep("dataFile", h2_ctl) + 1
line_modnm <- grep("modelName", h2_ctl) + 1

yr_prev <- as.numeric(format(Sys.time(), "%Y"))-1
yr_curr <- as.numeric(format(Sys.time(), "%Y"))
mod_new <- mod_prev

cv_cpue <- 0.2
cv_acousN <- .3

file_input <- "data/SC13_2025assessmentInputData_V2.xlsx" # From https://southpacificrfmo.sharepoint.com/:f:/r/sites/SPRFMOSCJackMackerelWorkingGroup/Shared%20Documents/Data%20repository
names_input <- excel_sheets(file_input)

# Age-0 fish are generally deleted from all the comp data.

dat_catch <- read_excel(file_input, sheet=5) %>%
  mutate_at(vars(-year), function(x)x/1e3) %>%
  rename_with(~ gsub("F", "fishery", .x, fixed = TRUE))

# Not sure if new system for SC13?
  # No not yet...
dat_wtatage_peru <- read_excel("data/WatAge_update_2025.xlsx", sheet=1) %>%
  rename(year = yy) %>%
  mutate(fleet=3) %>%
  filter(year>=yr_prev)

dat_wtatage_f <- read_excel(file_input, sheet=2) %>%
  pivot_longer(cols=contains("F"), names_to="fleet") %>%
  pivot_wider(names_from=age, values_from=value) %>%
  mutate(fleet=as.numeric(str_extract(fleet,"\\d"))) %>%
  select(-`0`) %>%
  filter(fleet!=3) %>%
  add_row(dat_wtatage_peru)

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
         err=ifelse(year == yr_curr, cv_cpue * index * 2, cv_cpue * index)
       )

dat_acous <- read_excel(file_input, sheet=6) %>%
  pivot_longer(contains("F"), names_to="fleet") %>%
  pivot_wider(names_from=type,values_from=value) %>%
  mutate(
    total_biomass = agecomp * wtatage,
    fleet = as.numeric(str_extract(fleet, "\\d"))
  ) %>%
  replace_na(list("agecomp" = 0,"total_biomass" = 0)) %>%
  filter(age > 0, fleet == 1)

#-----------
# 0.01 Update last year's catch
#-----------
catch_prev <- dat_catch %>%
              filter(year==yr_prev) %>%
              select(-year) %>%
              unlist()

mod_new[[1]]$data$Fcaton[which(rownames(mod_new[[1]]$data$Fcaton)==yr_prev),] <- catch_prev

# fn_bridge(mod_new, "0.01")
# mod0.01 <- runit(geth("0.01",c("h1","h2")),pdf=F,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)

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
# mod0.02 <- runit(geth("0.02",c("h1","h2")),pdf=F,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)
mod_prev <- mod_new


#----------
# 0.03 Last year's fishery (and CPUE) wtatage
#----------
mod_new <- mod_prev
cpue_ind <- grep("CPUE",mod_new[[1]]$data$Inames)

for(f in seq_along(mod_new[[1]]$data$Fnames)) {
  rows2use <- which(rownames(mod_new[[1]]$data$Fwtatage[,,f])==yr_prev)
  dat2use <- dat_wtatage_f %>%
               filter(year==yr_prev, fleet==f) %>%
               select(-year,-fleet) %>%
               unlist()

  for(a in seq_along(dat2use)) {
    mod_new[[1]]$data$Fwtatage[rows2use,a,f] <- ifelse(
      is.na(dat2use[a]),
      mod_new[[1]]$data$Fwtatage[rows2use,a,f],
      dat2use[a]
    )
  }

 # Update wtatage for CPUE
  if(f>1) {
    rows2use <- which(rownames(mod_new[[1]]$data$Iwtatage[,,cpue_ind[f-1]])==yr_prev)
    mod_new[[1]]$data$Iwtatage[rows2use,,cpue_ind[f-1]] <- mod_new[[1]]$data$Fwtatage[rows2use,,f]
  }
}

# fn_bridge(mod_new, "0.03")
# mod0.03 <- runit(geth("0.03",c("h1","h2")),pdf=F,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)
mod_prev <- mod_new

#----------
# 0.04 Previous year's offshore CPUE
#----------
mod_new <- mod_prev

i <- grep("Offshore",mod_new[[1]]$data$Inames)
dat2use <- dat_cpue %>% filter(fleet==4) %>%
  na.omit()
rows2use <- which(rownames(mod_new[[1]]$data$Index) %in% dat2use$year)

mod_new[[1]]$data$Inumyears[i] <- dim(dat2use)[1]
mod_new[[1]]$data$Index[,i] <- mod_new[[1]]$data$Indexerr[,i] <- mod_new[[1]]$data$Iyears[,i] <- NA # In case the years change... which they shouldn't.
mod_new[[1]]$data$Index[rows2use,i] <- dat2use$index
mod_new[[1]]$data$Indexerr[rows2use,i] <- dat2use$err
mod_new[[1]]$data$Iyears[rows2use,i] <- dat2use$year

# fn_bridge(mod_new, "0.04")
# mod0.04 <- runit(geth("0.04",c("h1","h2")),pdf=F,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)
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
# mod0.05 <- runit(geth("0.05",c("h1","h2")),pdf=T,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)


#----------
# 0.06 Add current year's fishery length and age comps
#----------
# Need to read in new files to get rownames etc.
mod_new <- readJJM("h1_0.05", path = "config", input = "input")
mod_prev <- mod_new

for(f in 1:mod_new[[1]]$data$Fnum) {

  if(mod_new[[1]]$data$FnumyearsA[f] > 0) {
    rows2use <- which(rownames(mod_new[[1]]$data$Fagecomp[,,f])==yr_curr)
    dat2use <- dat_agecomp_f %>%
              filter(year==yr_curr, fleet==f) %>%
              select(-year,-fleet) %>%
              unlist()
    if(length(dat2use)>0 & sum(dat2use)>0){
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
    if(length(dat2use)>0 & sum(dat2use > 0)){
      if(sum(is.na(mod_new[[1]]$data$Flengthcomp[rows2use,,f]))>0){
            mod_new[[1]]$data$FnumyearsL[f] <- mod_prev[[1]]$data$FnumyearsL[f] + 1
            mod_new[[1]]$data$Flengthyears[rows2use,f] <- yr_curr
      }
        mod_new[[1]]$data$Flengthcomp[rows2use,,f] <- dat2use
        mod_new[[1]]$data$Flengthsample[rows2use,f] <- mod_new[[1]]$data$Flengthsample[(rows2use-1),f] * .1
    }
  }
}

# fn_bridge(mod_new, "0.06")
# mod0.06 <- runit(geth("0.06",c("h1","h2")),pdf=F,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)
mod_prev <- mod_new


#----------
# 0.07 Current year's fishery (and CPUE) wtatage
#----------
mod_new <- mod_prev
cpue_ind <- grep("CPUE",mod_new[[1]]$data$Inames)

for(f in 1:mod_new[[1]]$data$Fnum) {
  rows2use <- which(rownames(mod_new[[1]]$data$Fwtatage[,,f])==yr_curr)
  dat2use <- dat_wtatage_f %>%
                filter(year==yr_curr, fleet==f) %>%
                select(-year,-fleet) %>%
                unlist()

    if(length(dat2use)>0 & sum(!is.na(dat2use))>0) {
      for(a in seq_along(dat2use)) {
        mod_new[[1]]$data$Fwtatage[rows2use,a,f] <- ifelse(
          is.na(dat2use[a]),
          mod_new[[1]]$data$Fwtatage[rows2use,a,f],
          dat2use[a]
        )
      }
    }

 # Update wtatage for CPUE
  if(f>1) {
    rows2use <- which(rownames(mod_new[[1]]$data$Iwtatage[,,cpue_ind[f-1]])==yr_curr)
    mod_new[[1]]$data$Iwtatage[rows2use,,cpue_ind[f-1]] <- mod_new[[1]]$data$Fwtatage[rows2use,,f]
  }
}

# fn_bridge(mod_new, "0.07")
mod0.07 <- runit(geth("0.07",c("h1","h2")),pdf=F,portrait=F,est=TRUE,exec="../src/jjms",parallel=T)

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
# mod0.08 <- runit(geth("0.08",c("h1","h2")),pdf=F,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)
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
# mod0.09 <- runit(geth("0.09",c("h1","h2")),pdf=F,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)
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
mod_new[[1]]$data$Indexerr[rows2use,i] <- mod_new[[1]]$data$Index[rows2use,i] * cv_acousN

# Update wt at age
rows2use <- which(rownames(mod_new[[1]]$data$Iwtatage[,,i]) %in% unique(dat_acous$year))
mod_new[[1]]$data$Iwtatage[rows2use,,i] <- tail(mod_new[[1]]$data$Iwtatage[,,i],6)[-6,] %>% colMeans()
mod_new[[1]]$data$Iwtatage[rows2use,!is.na(dat_acous$wtatage),i] <- dat_acous$wtatage[!is.na(dat_acous$wtatage)]

# Update agecomp
rows2use <- which(rownames(mod_new[[1]]$data$Ipropage[,,i]) %in% unique(dat_acous$year))

mod_new[[1]]$data$Ipropage[rows2use,,i] <- dat_acous$agecomp
mod_new[[1]]$data$Iagesample[rows2use,i] <- mod_new[[1]]$data$Iagesample[rev(which(!is.na(mod_new[[1]]$data$Iagesample[,i])))[1],i]


# fn_bridge(mod_new, "0.10")
# mod0.10 <- runit(geth("0.10",c("h1","h2")),pdf=F,portrait=F,est=TRUE,exec="../src/jjm",parallel=T)


#-------------
# 1.00 Update CTL
#-------------
mod_h1 <- readJJM(geth("0.10","h1"), path = "config", input = "input")
mod_h2 <- readJJM(geth("0.10","h2"), path = "config", input = "input")

tac_prev <- sum(tail(mod_h1[[1]]$data$Fcaton,1))

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

fn_update(mod_h1_new, "1.00", "h1")
fn_update(mod_h2_new, "1.00", "h2")

mod1.00 <- runit(geth("1.00",c("h1","h2")),pdf=T,portrait=F,est=TRUE,exec="../src/jjm",parallel=T, adflags=paste0("-tac ", tac_prev))
