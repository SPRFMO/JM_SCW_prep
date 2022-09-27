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

fn.update <- function(newmod, newmodname, h, dodat=T) {

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

basemod <- "1.01"

FinModName <- "1.05"

#----------
# Correcting growth parameters
#----------

h1_1.01 <- h1_1.00
h1_1.01[[1]]$data <- file.dat
h2_1.01 <- h2_1.00
h2_1.01[[1]]$data <- file.dat

h1_1.01[[1]]$control$Linf[1,1] <- h2_1.01[[1]]$control$Linf[1,] <- 73.56
h1_1.01[[1]]$control$Lo_len[1,1] <- h2_1.01[[1]]$control$Lo_len[1,] <- 13.56

# fn.update(h1_1.01, "1.01", "h1", dodat=F)
# fn.update(h2_1.01, "1.01", "h2", dodat=F)
h1_1.01 <- runit(geth("1.01","h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
h2_1.01 <- runit(geth("1.01","h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")


#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# Projection runs
#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# Now done in SC09_Projections.R

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

write.table(table_15, paste0("results/table_15_sce_",geth(FinModName,"h1"),".txt"), sep="\t")

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
mod_2stk <- readJJM(geth(FinModName,hyp),path="config",input="input")

SSB <- tibble(Year=mod_2stk[[1]]$output$Stock_1$SSB[,1],
              Two_Stock=mod_2stk[[1]]$output$Stock_1$SSB[,2]+mod_2stk[[1]]$output$Stock_2$SSB[,2],
              One_Stock=FinMod[[1]]$output$Stock_1$SSB[,2]) %>% 
        pivot_longer(-Year, names_to="Model",values_to="SSB") 

Rec <- tibble(Year=mod_2stk[[1]]$output$Stock_1$R[,1],
              Two_Stock=mod_2stk[[1]]$output$Stock_1$R[,2]+mod_2stk[[1]]$output$Stock_2$R[,2],
              One_Stock=FinMod[[1]]$output$Stock_1$R[,2]) %>% 
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
#-------------------------------------------
# library(foreach)
# library(doParallel)
# registerDoParallel(5)
# dir.jjmR <- "../../jjmR"
# dir.jjm <- getwd()
# setwd(dir.jjmR)
# devtools::load_all()
# setwd(dir.jjm)

FinMod <- geth(FinModName,"h1")
finmod.h1 <- readJJM(FinMod, path = "config", input = "input")
finmod.h1r <- finmod.h1
names(finmod.h1r) <- FinMod
Npeels<-5

# ret1 <- retro(model = finmod.h1r, n = Npeels, output = "results", exec="../src/jjms",parallel=T)

load(paste0("results/",FinMod,"_retrospective.RData"))
ret1<-output
pdf(paste0("results/",FinMod,"_Retro.pdf"))
plot(ret1) # all plots
dev.off()

plot(ret1, var="SSB") # only SSB
#install.packages("icesAdvice")
#Calculation of Mohn's Rho courtesy of Arni Magnusson
icesAdvice::mohn(ret1$Stock_1$SSB$var[,1,1:6],peel=5,details=T)



FinMod <- geth(FinModName,"h2")
finmod.h2 <- readJJM(FinMod, path = "config", input = "input")
finmod.h2r <- finmod.h2
names(finmod.h2r) <- FinMod
Npeels<-5

# ret2 <- retro(model = finmod.h2r, n = Npeels, output = "results", exec="../src/jjms",parallel=T)

load(paste0("results/",FinMod,"_retrospective.RData"))
ret2<-output
pdf(paste0("results/",FinMod,"_Retro.pdf"))
plot(ret2) # all plots
dev.off()

plot(ret2, var="SSB") # only SSB
#install.packages("icesAdvice")
#Calculation of Mohn's Rho courtesy of Arni Magnusson
icesAdvice::mohn(ret2$Stock_1$SSB$var[,1,1:6],peel=5,details=T)
icesAdvice::mohn(ret2$Stock_2$SSB$var[,1,1:6],peel=5,details=T)



