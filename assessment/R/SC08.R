# ------------------------------------------------------------------------
# Script for SC08---------------------------------------------------------
# 2020 JM  ---------------------------------------------------------------
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
fixed_bmsy <- function(mod,refpt=5500){
  old_rat <- (mod[[1]]$output[[1]]$msy_mt[,13])
  new_rat <- (mod[[1]]$output[[1]]$msy_mt[,12]/ refpt)
  mod[[1]]$output[[1]]$msy_mt[,13] <- new_rat
  mod[[1]]$output[[1]]$msy_mt[,10] <- refpt
  return(mod)
}

#-------------------------
# Model configuration runs
#-------------------------

# Check models are the same

# mod0.00 <- runit(geth("0.00"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

# setwd("../../jjm_2019/assessment/SC07") # only works on LQ's computer at the moment
# mod_prev <- readJJM("mod1.00", path = "config", input = "input")
# setwd(pwd)

mod0.00 <- readJJM(geth("0.00","h1"), path = "config", input = "input")
# loads last year's model as mod_prev
load("results/mod_prev_h1.Rdat")

oldnewMods <- combineModels(mod0.00,mod_prev)
plot(oldnewMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(oldnewMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(oldnewMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")

mod0.00 <- readJJM(geth("0.00","h2"), path = "config", input = "input")
# loads last year's model as mod_prev
load("results/mod_prev_h2.Rdat")

oldnewMods <- combineModels(mod0.00,mod_prev)
plot(oldnewMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(oldnewMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(oldnewMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")

#----------------
# Data increments
#----------------
# mod0.01 <- runit(geth("0.01"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.02 <- runit(geth("0.02"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.03 <- runit(geth("0.03"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.04 <- runit(geth("0.04"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.05 <- runit(geth("0.05"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.06 <- runit(geth("0.06"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.07 <- runit(geth("0.07"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.08 <- runit(geth("0.08"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.09 <- runit(geth("0.09"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.10 <- runit(geth("0.10"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.11 <- runit(geth("0.11"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.12 <- runit(geth("0.12"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.13 <- runit(geth("0.13"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

# mod0.00 <- readJJM(geth("0.00"), path = "config", input = "input")
# mod0.01 <- readJJM(geth("0.01"), path = "config", input = "input")
# mod0.02 <- readJJM(geth("0.02"), path = "config", input = "input")
# mod0.03 <- readJJM(geth("0.03"), path = "config", input = "input")
# mod0.04 <- readJJM(geth("0.04"), path = "config", input = "input")
# mod0.05 <- readJJM(geth("0.05"), path = "config", input = "input")
# mod0.06 <- readJJM(geth("0.06"), path = "config", input = "input")
# mod0.07 <- readJJM(geth("0.07"), path = "config", input = "input")
# mod0.08 <- readJJM(geth("0.08"), path = "config", input = "input")
# mod0.09 <- readJJM(geth("0.09"), path = "config", input = "input")
# mod0.10 <- readJJM(geth("0.10"), path = "config", input = "input")
# mod0.11 <- readJJM(geth("0.11"), path = "config", input = "input")
# mod0.12 <- readJJM(geth("0.12"), path = "config", input = "input")
# mod0.13 <- readJJM(geth("0.13"), path = "config", input = "input")

#--------------------
# Full model
#--------------------

# mod1.00 <- runit(geth("1.00"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

#h1_1.00 <- readJJM(geth("1.00","h1"), path = "config", input = "input")
#h2_1.00 <- readJJM(geth("1.00","h2"), path = "config", input = "input")


#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# Projection runs
#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
#------------
# Risk Tables
# h1
#------------

FinModName <- "1.00"
FinMod <- readJJM(geth(FinModName,"h1"),path="config",input="input")

# modhl  <- runit(paste0(geth(FinModName,"h1"),".hl"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modll  <- runit(paste0(geth(FinModName,"h1"),".ll"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modhs  <- runit(paste0(geth(FinModName,"h1"),".hs"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modls  <- runit(paste0(geth(FinModName,"h1"),".ls"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

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




