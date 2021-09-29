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

fn.update <- function(newmod, newmodname, h) {

  names(newmod) <- newmod[[1]]$control$modelName <- geth(newmodname,h)
  newmod[[1]]$control$dataFile <- paste0(newmodname,".dat")
  
  writeJJM(newmod,datPath="input",ctlPath="config")
}

#-------------------------
# Model configuration runs
#-------------------------

# Bridging now done in SC09_Bridging.R

#--------------------
# Full model
#--------------------

# mod1.00 <- runit(geth("1.00"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

h1_1.00 <- readJJM(geth("1.00","h1"), path = "config", input = "input")
h2_1.00 <- readJJM(geth("1.00","h2"), path = "config", input = "input")

basemod <- "1.01"
#----------
# Loading updated Chilean data
# Still need to include 2021 data
#----------
file.dat <- h1_1.00[[1]]$data

dat.acousN.nue <- read_csv(here("data","SC09_AcousN_Nuevo.csv")) %>%
                      janitor::clean_names() %>%
                      mutate(numbers_at_age=as.numeric(str_remove_all(replace_na(numbers_at_age,0)," ")),
                              total_biomass=replace_na(total_biomass,0)) %>%
                      filter(age>0) %>%
                      rename(catch_no=numbers_at_age,
                              avg_wt=mean_weight,
                              catch_wt=total_biomass) %>%
                      mutate(fleet_type="survey",
                              fleet=1,
                              age=as.numeric(str_extract(age, "[0-9]+")),
                              method="validacion") %>%
                      select(-mean_length)

dat.f.new <- read_csv(here("data","SC09_catchNumByFleet_newAgeMet.csv")) %>% 
                      select(-1,-`0`) %>%
                      mutate_all(replace_na,0) %>%
                      mutate(fleet=as.numeric(str_extract(fleet,"\\d"))) %>%
                      pivot_longer(-(year:fleet),names_to="age",values_to="catch_no") %>%
                      mutate(fleet_type="fishery", 
                              age=as.numeric(str_extract(age, "[0-9]+"))) %>%
                      left_join(
                        read_csv(here("data","SC09_catchWtByFleet_newAgeMet.csv")) %>%
                        select(-1,-`0`)  %>%
                        mutate(fleet=as.numeric(str_extract(fleet,"\\d"))) %>%
                        pivot_longer(-(year:fleet),names_to="age",values_to="avg_wt") %>%
                        mutate(fleet_type="fishery", 
                                age=as.numeric(str_extract(age, "[0-9]+")),
                                method="validacion")
                    ) %>%
                      filter(fleet %in% 1:2)

dat.chile <- read_csv(here::here("data","NewAgeData","All_Age_Data_Chile.csv")) %>%
              janitor::clean_names() %>%
              rename(year=ano,
                      fleet=macrozona,
                      age=grupo_edad,
                      catch_no=capt,
                      catch_wt=capt_ton,
                      method=criterio_lectura) %>%
              mutate(fleet_type=ifelse(grepl("acoustic",fleet),"survey","fishery"),
                      fleet=as.numeric(str_extract(fleet, "[0-9]+"))) %>%
              group_by(fleet,method,year,age,fleet_type) %>%
              summarise(catch_no=sum(catch_no), 
                        catch_wt=sum(catch_wt),
                        avg_wt=1e3*catch_wt/catch_no) %>%
              ungroup() %>%
              bind_rows(dat.acousN.nue,dat.f.new) %>%
              filter(age!=0) %>%
              complete(age, nesting(fleet, method, year, fleet_type))

# Check if dat.f.new data match up with dat.chile for 2020.. doesn't seem to be.
# Also current code creates duplicates rows
# Where did newAgeMet come from? Is it revised antiguo or validacion?

dat.fish <- filter(dat.chile, fleet_type=="fishery")

dat.ind <- filter(dat.chile, fleet_type=="survey")

for(f in unique(dat.fish$fleet)) {

  dat2use <- dat.fish %>%
            filter(fleet==f, method=="antiguo") %>%
            select(-fleet, -method, -fleet_type)
  rows2use <- rownames(file.dat$Fagecomp[,,f]) %in% dat2use$year
  file.dat$Fagecomp[rows2use,,f] <- dat2use %>%
                                      select(-catch_wt, -avg_wt) %>%
                                      pivot_wider(names_from=age,
                                                  values_from=catch_no) %>%
                                      select(str_sort(names(.),numeric=T), -year) %>% 
                                      mutate_all(replace_na,0) %>%
                                      as.matrix()
  # file.dat$Fagesample[!(rows2use|is.na(file.dat$Fagesample[,f])),f] <- file.dat$Fagesample[rows2use,f][1]*.1

  tmp <- dat2use %>%
          select(-catch_wt, -catch_no) %>%
          pivot_wider(names_from=age,
                      values_from=avg_wt) %>%
          select(str_sort(names(.),numeric=T),-year) %>% 
          as.matrix()

  tmp[is.na(tmp)] <- file.dat$Fwtatage[rows2use,,f][is.na(tmp)]
  file.dat$Fwtatage[rows2use,,f] <- tmp

  # Update wtatage for CPUE
  if(f>1) {
    ind <- grep("Chile_CPUE",file.dat$Inames)
    file.dat$Iwtatage[,,ind] <- file.dat$Fwtatage[,,f]
  }
}

for(f in unique(dat.ind$fleet)) {
  if(f==1) ff=2 # Index 2 == Chile_AcousN in assessment
  if(f==2) ff=1 # Index 1 == Chile_AcousCS in assessment

  dat2use <- dat.ind %>%
              filter(fleet==f, method=="antiguo") %>%
              select(-fleet, -method, -fleet_type) %>%
              filter(year %in% file.dat$Iyears[,ff])
  rows2use <- rownames(file.dat$Ipropage[,,ff]) %in% dat2use$year
  file.dat$Ipropage[rows2use,,ff] <- dat2use %>%
                                      select(-catch_wt, -avg_wt) %>%
                                      pivot_wider(names_from=age,
                                                  values_from=catch_no) %>%
                                      mutate(`1`=0) %>%
                                      select(str_sort(names(.),numeric=T),-year) %>% 
                                      mutate_all(replace_na,0) %>%
                                      as.matrix()
  # file.dat$Iagesample[!(rows2use|is.na(file.dat$Iagesample[,ff])),ff] <- file.dat$Iagesample[rows2use,ff][1]*.1 
  # AcousN missing 2008, 2010, 2011, 2012, 2021
  # AcousCS has extra 2010, 2011, 2012, 2017, 2020 age comps, but we don't have the index of abundance for those years (ends in 2009)

  tmp <- dat2use %>%
          select(-catch_wt, -catch_no) %>%
          pivot_wider(names_from=age,
                      values_from=avg_wt) %>%
          select(str_sort(names(.),numeric=T),-year) %>% 
          as.matrix()

  tmp[is.na(tmp)] <- file.dat$Iwtatage[rows2use,,ff][is.na(tmp)]
  file.dat$Iwtatage[rows2use,,ff] <- tmp

}

h1_1.01 <- h1_1.00
h1_1.01[[1]]$data <- file.dat
h2_1.01 <- h2_1.00
h2_1.01[[1]]$data <- file.dat

# fn.update(h1_1.01, "1.01", "h1")
# fn.update(h2_1.01, "1.01", "h2")
# h1_1.01 <- runit(geth("1.01",h="h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# h2_1.01 <- runit(geth("1.01",h="h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

#---------
# Using the "validacion" data set
# Still need to update M and maturity parameters
# Update 2021 data points
#---------
file.dat <- h1_1.00[[1]]$data
for(f in unique(dat.fish$fleet)) {

  dat2use <- dat.fish %>%
            filter(fleet==f, method=="validacion") %>%
            select(-fleet, -method, -fleet_type)
  rows2use <- rownames(file.dat$Fagecomp[,,f]) %in% dat2use$year
  file.dat$Fagecomp[rows2use,,f] <- dat2use %>%
                                      select(-catch_wt, -avg_wt) %>%
                                      pivot_wider(names_from=age,
                                                  values_from=catch_no) %>%
                                      select(str_sort(names(.),numeric=T), -year) %>% 
                                      mutate_all(replace_na,0) %>%
                                      as.matrix()
  # file.dat$Fagesample[!(rows2use|is.na(file.dat$Fagesample[,f])),f] <- file.dat$Fagesample[rows2use,f][1]*.1

  tmp <- dat2use %>%
          select(-catch_wt, -catch_no) %>%
          pivot_wider(names_from=age,
                      values_from=avg_wt) %>%
          select(str_sort(names(.),numeric=T),-year) %>% 
          as.matrix()

  tmp[is.na(tmp)] <- file.dat$Fwtatage[rows2use,,f][is.na(tmp)]
  file.dat$Fwtatage[rows2use,,f] <- tmp

  # Update wtatage for CPUE
  if(f>1) {
    ind <- grep("Chile_CPUE",file.dat$Inames)
    file.dat$Iwtatage[,,ind] <- file.dat$Fwtatage[,,f]
  }
}

for(f in unique(dat.ind$fleet)) {
  if(f==1) ff=2 # Index 2 == Chile_AcousN in assessment
  if(f==2) ff=1 # Index 1 == Chile_AcousCS in assessment

  dat2use <- dat.ind %>%
              filter(fleet==f, method=="validacion") %>%
              select(-fleet, -method, -fleet_type) %>%
              filter(year %in% file.dat$Iyears[,ff])
  rows2use <- rownames(file.dat$Ipropage[,,ff]) %in% dat2use$year
  file.dat$Ipropage[rows2use,,ff] <- dat2use %>%
                                      select(-catch_wt, -avg_wt) %>%
                                      pivot_wider(names_from=age,
                                                  values_from=catch_no) %>%
                                      mutate(`1`=0) %>%
                                      select(str_sort(names(.),numeric=T),-year) %>% 
                                      mutate_all(replace_na,0) %>%
                                      as.matrix()
  # file.dat$Iagesample[!(rows2use|is.na(file.dat$Iagesample[,ff])),ff] <- file.dat$Iagesample[rows2use,ff][1]*.1 
  # AcousN missing 2008, 2010, 2011, 2012, 2021
  # AcousCS has extra 2010, 2011, 2012, 2017, 2020 age comps, but we don't have the index of abundance for those years (ends in 2009)

  tmp <- dat2use %>%
          select(-catch_wt, -catch_no) %>%
          pivot_wider(names_from=age,
                      values_from=avg_wt) %>%
          select(str_sort(names(.),numeric=T),-year) %>% 
          as.matrix()

  tmp[is.na(tmp)] <- file.dat$Iwtatage[rows2use,,ff][is.na(tmp)]
  file.dat$Iwtatage[rows2use,,ff] <- tmp

}



h1_1.02 <- h1_1.00
h1_1.02[[1]]$data <- file.dat
h2_1.02 <- h2_1.00
h2_1.02[[1]]$data <- file.dat

# fn.update(h1_1.02, "1.02", "h1")
# fn.update(h2_1.02, "1.02", "h2")
# h1_1.02 <- runit(geth("1.02",h="h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# h2_1.02 <- runit(geth("1.02",h="h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")


#---------
# 1.04
# Downweighting final year for every CPUE index to reflect increased uncertainty
#---------
mod.base.h1 <- readJJM(geth(basemod,"h1"), path = "config", input = "input")
mod.base.h2 <- readJJM(geth(basemod,"h2"), path = "config", input = "input")

CV.CPUE.fin <- .4
cpue_ind <- grep("CPUE",mod.base.h1[[1]]$data$Inames)

for(i in cpue_ind) {
  row2use <- max(names(mod.base.h1[[1]]$data$Indexerr[,i])[!is.na(mod.base.h1[[1]]$data$Indexerr[,i])])
  mod.base.h1[[1]]$data$Indexerr[row2use,i] <- mod.base.h1[[1]]$data$Index[row2use,i] * CV.CPUE.fin
}

h1_1.04 <- mod.base.h1
h1_1.04[[1]]$data <- mod.base.h1[[1]]$data
h2_1.04 <- mod.base.h2
h2_1.04[[1]]$data <- mod.base.h1[[1]]$data

# fn.update(h1_1.04, "1.04", "h1")
# fn.update(h2_1.04, "1.04", "h2")
# h1_1.04 <- runit(geth("1.04",h="h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# h2_1.04 <- runit(geth("1.04",h="h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

#------
# 1.05
# Fixing 2020/2021 wt at age for N_Chile
#------

h1_1.05 <- h1_1.04
h2_1.05 <- h2_1.04

file.dat <- h1_1.05[[1]]$data
f2change <- grep("N_Chile", file.dat$Fnames)
yrs2change <- c(2020,2021)
yrs2change2 <- 2019

row2use <- which(rownames(file.dat$Fwtatage[,,f2change]) %in% yrs2change)
row2change2 <- which(rownames(file.dat$Fwtatage[,,f2change]) == yrs2change2)
file.dat$Fwtatage[row2use,,f2change] <- matrix(rep(file.dat$Fwtatage[row2change2,,f2change],times=length(yrs2change)),nrow=length(yrs2change),byrow=T)

h1_1.05[[1]]$data <- file.dat
h2_1.05[[1]]$data <- file.dat

# fn.update(h1_1.05, "1.05", "h1")
# fn.update(h2_1.05, "1.05", "h2")
# h1_1.05 <- runit(geth("1.05",h="h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# h2_1.05 <- runit(geth("1.05",h="h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# Projection runs
#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
#------------
# Risk Tables
# h1
#------------


h1_modhl <- readJJM(paste0(geth(FinModName,"h1"),".hl"),path="config",input="input")
h1_modll <- readJJM(paste0(geth(FinModName,"h1"),".ll"),path="config",input="input")
h1_modhs <- readJJM(paste0(geth(FinModName,"h1"),".hs"),path="config",input="input")
h1_modls <- readJJM(paste0(geth(FinModName,"h1"),".ls"),path="config",input="input")

# Only do this for h1
# Should not be setting BMSY for h2
# Most of this is setting BMSY at the interim level

h1_modls[[1]]$output[[1]]$msy_mt[,13] <- h1_modls[[1]]$output[[1]]$msy_mt[,12]/ 5500
h1_modls[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(h1_modls, format="word", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))
report(h1_modls, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

h1_modhs[[1]]$output[[1]]$msy_mt[,13] <- h1_modhs[[1]]$output[[1]]$msy_mt[,12]/ 5500
h1_modhs[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(h1_modhs, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

h1_modhl[[1]]$output[[1]]$msy_mt[,13] <- h1_modhl[[1]]$output[[1]]$msy_mt[,12]/ 5500
h1_modhl[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(h1_modhl, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

h1_modll[[1]]$output[[1]]$msy_mt[,13] <- h1_modll[[1]]$output[[1]]$msy_mt[,12]/ 5500
h1_modll[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(h1_modll, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

kobe(h1_modls)
FinMod_msy <- fixed_bmsy(FinMod)
FinMod_msy
kobe(FinMod_msy)

report(FinMod_msy, format="word", output="risk_tables/")

# 20 year projection table
summary(FinMod_msy, Projections=TRUE, Fmult=c(0, "FMSY", .75, 1, 1.25))

#-------------------
# Risk Tables for h2
#-------------------

FinMod <- readJJM(geth(FinModName,"h2"),path="config",input="input")

# modhl  <- runit(paste0(geth(FinModName,"h2"),".hl"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modll  <- runit(paste0(geth(FinModName,"h2"),".ll"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modhs  <- runit(paste0(geth(FinModName,"h2"),".hs"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modls  <- runit(paste0(geth(FinModName,"h2"),".ls"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

h2_modhl <- readJJM(paste0(geth(FinModName,"h2"),".hl"),path="config",input="input")
h2_modll <- readJJM(paste0(geth(FinModName,"h2"),".ll"),path="config",input="input")
h2_modhs <- readJJM(paste0(geth(FinModName,"h2"),".hs"),path="config",input="input")
h2_modls <- readJJM(paste0(geth(FinModName,"h2"),".ls"),path="config",input="input")

report(h2_modls, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))
report(h2_modls, format="word", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

report(h2_modhs, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

report(h2_modhl, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

report(h2_modll, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

report(FinMod, format="word", output="risk_tables/")

kobe(h2_modls)

# 20 year projection table
summary(h2_modls, Projections=TRUE, Fmult=c(0, "FMSY", .75, 1, 1.25))


#--------------------------
# Tables for the Tech Annex
# Juan Carlos Quiroz's tables
# Loads packages
#--------------------------

source("R/constructor_input_tables.R")

construct_input_tables(modname=geth(FinModName,"h1"),docname="Input_Tables_SC08")

# Summary data
FinMod <- readJJM(geth(FinModName,"h1"),path="config",input="input")
write.csv(summary(FinMod)$like,"results/SummaryLikelihoods_h1.csv")

FinMod <- readJJM(geth(FinModName,"h2"),path="config",input="input")
write.csv(summary(FinMod)$like,"results/SummaryLikelihoods_h2.csv")


#-------------------
# Table 15
# Derived Quantities
#-------------------
FinMod <- readJJM(geth(FinModName,"h1"),path="config",input="input")
y_ind <- FinMod[[1]]$output[[1]]$SSB[,1] %in% FinMod[[1]]$output[[1]]$msy_mt[,1]
table_15 <- data.frame(yr  = FinMod[[1]]$output[[1]]$msy_mt[,1]) %>% 
                  mutate(
                    yflo      = round(as.vector(rowSums(FinMod[[1]]$data$Fcaton)), 0),
                    ssb       = round(FinMod[[1]]$output[[1]]$SSB[y_ind,2], 0),
                    r         = round(FinMod[[1]]$output[[1]]$R[,2], 0),
                    ssb_msy   = round(FinMod[[1]]$output[[1]]$msy_mt[,10], 0),
                    fflo      = round(FinMod[[1]]$output[[1]]$msy_mt[,6], 2),
                    fmsy      = round(FinMod[[1]]$output[[1]]$msy_mt[,5], 2),
                    ssbmsy    = round(FinMod[[1]]$output[[1]]$msy_mt[,10], 2),
                    r_fmsy    = round(FinMod[[1]]$output[[1]]$msy_mt[,4], 2),
                    r_ssbmsy  = round(FinMod[[1]]$output[[1]]$msy_mt[,13], 2)
                    )

write.table(table_15, paste0("table_15_sce_",geth(FinModName,"h1"),".txt"), sep="\t")

# m <- data.frame(N=modls[[1]]$output$Stock_1$N[49,2:13],age=1:12,Year_Estimated=2019)
# m <- rbind(m,data.frame(N=mod1.00[[1]]$output$Stock_1$N[49,2:13],age=1:12,Year_Estimated=2019))
# m$Year_Estimated <- as.factor(m$Year_Estimated)


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

#---------------------------
# Plot recruitment scenarios
#---------------------------
modls <- h1_modls
modll <- h1_modll
modhs <- h1_modhs
modhl <- h1_modhl
mod0.00 <- readJJM(geth("0.00","h1"),path="config",input="input")

mdf <- data.frame(Model="Low steepness, recent series",modls[[1]]$output[[1]]$SSB_fut_1)
mdf <- rbind(mdf,data.frame(Model="Low steepness, recent series",modls[[1]]$output[[1]]$SSB))
mdf <- rbind(mdf,data.frame(Model="Low steepness, full series",modll[[1]]$output[[1]]$SSB_fut_1))
mdf <- rbind(mdf,data.frame(Model="Low steepness, full series",modll[[1]]$output[[1]]$SSB))
mdf <- rbind(mdf,data.frame(Model="High steepness, recent series",modhs[[1]]$output[[1]]$SSB_fut_1))
mdf <- rbind(mdf,data.frame(Model="High steepness, recent series",modhs[[1]]$output[[1]]$SSB))
mdf <- rbind(mdf,data.frame(Model="High steepness, full series",modhl[[1]]$output[[1]]$SSB_fut_1))
mdf <- rbind(mdf,data.frame(Model="High steepness, full series",modhl[[1]]$output[[1]]$SSB))

mdf <- data.frame(Model="2020 assessment",modls[[1]]$output[[1]]$SSB_fut_1)
mdf <- rbind(mdf,data.frame(Model="2020 assessment",modls[[1]]$output[[1]]$SSB))
mdf <- rbind(mdf,data.frame(Model="2019 assessment",mod0.00[[1]]$output[[1]]$SSB_fut_1))
mdf <- rbind(mdf,data.frame(Model="2019 assessment",mod0.00[[1]]$output[[1]]$SSB))

names(mdf) <- c("Scenario", "Year","SSB","SD","lb","ub")
source("R/prelims.R")
ggplot(mdf,aes(x=Year,y=SSB,fill=Scenario,color=Scenario)) + xlim(2010,2026) + ylim(0,15000) + geom_line() + 
  geom_ribbon(aes(ymin=lb,ymax=ub),alpha=.3) + mytheme + geom_hline(yintercept=5500,linetype="dashed")+ geom_vline(xintercept=2021,linetype="dashed")

#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000


hyp <- "h2"
mod_2stk <- readJJM(geth(FinModName),path="config",input="input")

SSB <- tibble(Year=mod_2stk$Model_1.0$output$Stock_1$SSB[,1],
              Two_Stock=mod_2stk$Model_1.0$output$Stock_1$SSB[,2]+mod_2stk$Model_1.0$output$Stock_2$SSB[,2],
              One_Stock=FinMod$Model_1.0$output$Stock_1$SSB[,2]) %>% 
        pivot_longer(-Year, names_to="Model",values_to="SSB") 

Rec <- tibble(Year=mod_2stk$Model_1.0$output$Stock_1$R[,1],
              Two_Stock=mod_2stk$Model_1.0$output$Stock_1$R[,2]+mod_2stk$Model_1.0$output$Stock_2$R[,2],
              One_Stock=FinMod$Model_1.0$output$Stock_1$R[,2]) %>% 
        pivot_longer(-Year, names_to="Model",values_to="Recruitment") 

Full_tib <- left_join(SSB,Rec, by=c("Year","Model")) %>%
              pivot_longer(-(Year:Model),names_to="Quantity",values_to="Value") %>%
              group_by(Model,Quantity) %>%
              ggplot(mapping=aes(x=Year,y=Value,colour=Model)) + geom_line() + facet_wrap(~Quantity,ncol=1,scales="free_y") +
              mytheme + 
          geom_hline(aes(yintercept=5500,alpha=.5))
Full_tib


#-----------

Mod <- tibble(Year=modhl[[1]]$output$Stock_1$SSB[,1],
              High_Productivity=modhl[[1]]$output$Stock_1$SSB[,2],
              Low_Productivity=modls[[1]]$output$Stock_1$SSB[,2]) %>% 
        pivot_longer(-Year, names_to="Model",values_to="SSB") %>%
        mutate(Model=str_replace(Model,"_", " ")) %>%
        group_by(Model)

ggplot(Mod) + geom_line(mapping=aes(x=Year,y=SSB,colour=Model)) + facet_wrap(~Model,ncol=1) +
          theme_minimal(base_size=16) +
          theme(legend.position="none")



#--------------------------
# Generate summary table for historical retro
#--------------------------

final.modname <- readJJM(geth(FinModName,"h1"),path="config",input="input")   # current year assessment
repfile <- final.modname[[1]]$output[[1]]
assessmenttype <- "update"

old.table <- read.csv("hist_retro/SPRFMO historical retro.csv",header = T)
vars2pull <- colnames(old.table)
Nvars <- length(vars2pull)
Nyrs <- length(repfile$Yr)
sumtable <- matrix(NA, nrow = Nyrs, ncol = Nvars)
colnames(sumtable) <- vars2pull

old.table<-old.table[!old.table[,"Assessmentyear"] %in% max(repfile$Yr),]

sumtable[,"Assessmentyear"] <- max(repfile$Yr)
sumtable[,"Year"] <- repfile$Yr
sumtable[,grep("SSB", vars2pull)] <- repfile$SSB[repfile$SSB[,1] %in% repfile$Yr,2:5]
sumtable[,grep("R", vars2pull)] <- repfile$R[repfile$R[,1] %in% repfile$Yr,2:5]
sumtable[,grep("F1",vars2pull)[1]:grep("F12",vars2pull)] <- repfile$TotF
sumtable[,"F.Fmsy"] <- repfile$msy_mt[,4]
sumtable[,"Fmsy"] <- repfile$msy_mt[,5]
sumtable[,"Favg"] <- repfile$msy_mt[,6]
sumtable[,"Bmsy"] <- repfile$msy_mt[,10]
sumtable[,"B.Bmsy"] <- repfile$msy_mt[,13]
sumtable[,"assessmenttype"] <- assessmenttype

# if current assessment is larger than data contained in the 
if(max(old.table$Assessmentyear)>=max(repfile$Yr) & assessmenttype == "assessment") {
  temp <- old.table[-which(old.table$Assessmentyear == max(old.table$Assessmentyear)),]
  new.table <- rbind(temp, sumtable)
}

if(max(old.table$Assessmentyear)<max(repfile$Yr) | assessmenttype != "assessment") {
  new.table <- rbind(old.table, sumtable)
}

write.csv(as.data.frame(new.table), file = "SPRFMO historical retro.csv", quote = FALSE,row.names = F)

#-------------------------------------------
# mod1.0_r: Retrospective analysis on mod1.0
# Code won't work unless go into jjmR directory
# and do devtools::load_all()
# doesn't work in current stage..
#-------------------------------------------
#library(foreach)
#library(doParallel)
#registerDoParallel(5)
#dir.jjmR <- "../../jjmR"
#dir.jjm <- getwd()
#setwd(dir.jjmR)
#devtools::load_all()
#setwd(dir.jjm)

FinMod <- geth(FinModName,"h1")
#mod1.00 <- readJJM(FinModName, path = "config", input = "input")
#mod1.00r <- mod1.00
#names(mod1.00r) <- FinModName
#Npeels<-5
#
#ret1 <- retro(model = mod1.00r, n = Npeels, output = "results", exec="../src/jjms",parallel=T)

load(paste0("results/",FinMod,"_retrospective.RData"))
ret1<-output
pdf(paste0("results/",FinMod,"_Retro.pdf"))
plot(ret1) # all plots
dev.off()

plot(ret1, var="SSB") # only SSB
#install.packages("icesAdvice")
#Calculation of Mohn's Rho courtesy of Arni Magnusson
icesAdvice::mohn(ret1$Stock_1$SSB$var[,1,1:6],peel=5,details=T)




