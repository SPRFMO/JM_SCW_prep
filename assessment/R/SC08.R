# ------------------------------------------------------------------------
# Script for SC08---------------------------------------------------------
# 2020 JM  ---------------------------------------------------------------
# ------------------------------------------------------------------------
#install.packages("devtools")
#
# MAKE SURE TO UPDATE
# devtools::install_github("sprfmo/jjmr")
# Remember to recompile jjms as needed

library(jjmR)

#--------------------------------------------------------
# Working directory should be in assessment folder of jjm
#--------------------------------------------------------
# setwd(file.path(getwd(), "assessment"))
pwd <- getwd()
if (!grepl(basename(pwd), "assessment", ignore.case = TRUE)) {
  stop(paste("Set working directory to jjm/assessment"))
}
geth <- function(mod,h=hyp) paste0(h,"_", mod) # Package? Or keep?

#-------------------------
# Model configuration runs
#-------------------------

# Check models are the same
hyp <- "h1"

# mod0.00 <- runit(geth("0.00"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod0.00 <- readJJM(geth("0.00"), path = "config", input = "input")
setwd("../../jjm_2019/assessment/SC07")
mod_prev <- readJJM("mod1.00", path = "config", input = "input")
setwd(pwd)

oldnewMods <- combineModels(mod0.00,mod_prev)
plot(oldnewMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(oldnewMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(oldnewMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")


#----------------
# Data increments
#----------------
 mod0.01 <- runit(geth("0.01"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
 mod0.02 <- runit(geth("0.02"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
 mod0.03 <- runit(geth("0.03"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
 mod0.04 <- runit(geth("0.04"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
 mod0.05 <- runit(geth("0.05"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
 mod0.06 <- runit(geth("0.06"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
 mod0.07 <- runit(geth("0.07"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
 mod0.08 <- runit(geth("0.08"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
 mod0.09 <- runit(geth("0.09"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
 mod0.10 <- runit(geth("0.10"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
 mod0.11 <- runit(geth("0.11"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
 mod0.12 <- runit(geth("0.12"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.13<- runit(geth("0.13"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.14<- runit(geth("0.14"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.15<- runit(geth("0.15"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.16<- runit(geth("0.16"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.17<- runit(geth("0.17"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.18<- runit(geth("0.18"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

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
# mod0.14 <- readJJM(geth("0.14"), path = "config", input = "input")
# mod0.15 <- readJJM(geth("0.15"), path = "config", input = "input")
# mod0.16 <- readJJM(geth("0.16"), path = "config", input = "input")
# mod0.17 <- readJJM(geth("0.17"), path = "config", input = "input")
# mod0.18 <- readJJM(geth("0.18"), path = "config", input = "input")


CatchMods <- compareModels(geth(c("0.00","0.01")))
plot(CatchMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(CatchMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(CatchMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")



#--------------------
# Full model
#--------------------
# mod1.00 <- runit(geth("1.00"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

mod1.00 <- readJJM("mod1.00", path = "config", input = "input")

cbind(summary(mod0.16)$like,summary(mod0.17)$like)

#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# Projection runs
#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
FinMod <- mod1.00
FinModName <- "mod1.00"

# modhl  <- runit(paste0(FinModName,".hl"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modll  <- runit(paste0(FinModName,".ll"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modhs  <- runit(paste0(FinModName,".hs"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modls  <- runit(paste0(FinModName,".ls"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modls_2stk  <- runit(paste0(FinModName,".ls","_2stk"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")


modhl <- readJJM(paste0(FinModName,".hl"),path="config",input="input")
modll <- readJJM(paste0(FinModName,".ll"),path="config",input="input")
modhs <- readJJM(paste0(FinModName,".hs"),path="config",input="input")
modls <- readJJM(paste0(FinModName,".ls"),path="config",input="input")
library(PBSadmb)
# Substitute in adjusted N-age-2019 results to modls$outputo
m_ls <- readList("adjN/For_R_1.rep")    
modls_adj <- modls
modls_adj[[1]]$output$Stock_1 <- m_ls    

recmods <- combineModels(modhl,modhs,modll,modls)
recmods <- changeNameModel(recmods,c( "h=0.8, full series","h=0.8, short series","h=0.65, full series","h=0.65, short series" ))

# Function to put a constant reference point into image
fixed_bmsy <- function(mod,refpt=5500){
  old_rat <- (mod[[1]]$output[[1]]$msy_mt[,13])
  new_rat <- (mod[[1]]$output[[1]]$msy_mt[,12]/ refpt)
  mod[[1]]$output[[1]]$msy_mt[,13] <- new_rat
  mod[[1]]$output[[1]]$msy_mt[,10] <- refpt
  return(mod)
}

library(tidyverse)
mod <- mod0.17 #mod1.00

table_15 <- data.frame(yr  = mod[[1]]$output[[1]]$msy_mt[,1]) %>% 
                  mutate(
                    yflo      = round(as.vector(rowSums(mod[[1]]$data$Fcaton)), 0),
                    ssb       = round(mod[[1]]$output[[1]]$SSB[14:63,2], 0),
                    r         = round(mod[[1]]$output[[1]]$R[,2], 0),
                    ssb_msy   = round(mod[[1]]$output[[1]]$msy_mt[,10], 0),
                    fflo      = round(mod[[1]]$output[[1]]$msy_mt[,6], 2),
                    fmsy      = round(mod[[1]]$output[[1]]$msy_mt[,5], 2),
                    ssbmsy    = round(mod[[1]]$output[[1]]$msy_mt[,10], 2),
                    r_fmsy    = round(mod[[1]]$output[[1]]$msy_mt[,4], 2),
                    r_ssbmsy  = round(mod[[1]]$output[[1]]$msy_mt[,13], 2)
                    )

write.table(table_15, "table_15_sce_mod0.17.txt", sep="\t")


m <-         data.frame(N=modls[[1]]$output$Stock_1$N[49,2:13],age=1:12,Year_Estimated=2019)
m <- rbind(m,data.frame(N=mod1.00[[1]]$output$Stock_1$N[49,2:13],age=1:12,Year_Estimated=2019))
m$Year_Estimated <- as.factor(m$Year_Estimated)

library(ggplot2)
library(ggthemes)
ggplot(m,aes(y=N,x=age,fill=Year_Estimated)) + theme_few(base_size=12) + geom_bar(stat="identity",position="dodge") + 
  ggtitle("Numbers at age in 2018")

m <-         data.frame(N=modls[[1]]$output$Stock_1$N[49,2:13],age=1:12,Method="As estimated")
m <- rbind(m,data.frame(N=m_ls$N[49,2:13],age=1:12,Method="Adjusted N"))
ggplot(m,aes(y=N,x=age,fill=Method)) + theme_few(base_size=12) + geom_bar(stat="identity",position="dodge") + 
  ggtitle("Numbers at age in 2018")

# Kobe for main doc
kobe(FinMod)
kobe(fixed_bmsy(FinMod) ,add=T, col = 'red') 

main.diag <- diagnostics(fixed_bmsy(FinMod),plots=F)
plot(main.diag, var = "summarySheet")

main.diag <- diagnostics(FinMod,plots=F)
plot(main.diag, var = "summarySheet")



library(ggplot2)
(modll[[1]]$output$Stock_1$SSB)
(modll[[1]]$output$Stock_1$SSB)
mdf <- data.frame(Model="Low steepness, recent series",modls[[1]]$output[[1]]$SSB_fut_1)
mdf <- rbind(mdf,data.frame(Model="Low steepness, recent series",modls[[1]]$output[[1]]$SSB))
mdf <- rbind(mdf,data.frame(Model="Low steepness, full series",modll[[1]]$output[[1]]$SSB_fut_1))
mdf <- rbind(mdf,data.frame(Model="Low steepness, full series",modll[[1]]$output[[1]]$SSB))
mdf <- rbind(mdf,data.frame(Model="High steepness, recent series",modhs[[1]]$output[[1]]$SSB_fut_1))
mdf <- rbind(mdf,data.frame(Model="High steepness, recent series",modhs[[1]]$output[[1]]$SSB))
mdf <- rbind(mdf,data.frame(Model="High steepness, full series",modhl[[1]]$output[[1]]$SSB_fut_1))
mdf <- rbind(mdf,data.frame(Model="High steepness, full series",modhl[[1]]$output[[1]]$SSB))

mdf <- data.frame(Model="2019 assessment",modls[[1]]$output[[1]]$SSB_fut_1)
mdf <- rbind(mdf,data.frame(Model="2019 assessment",modls[[1]]$output[[1]]$SSB))
mdf <- rbind(mdf,data.frame(Model="2018 assessment",mod0.00[[1]]$output[[1]]$SSB_fut_1))
mdf <- rbind(mdf,data.frame(Model="2018 assessment",mod0.00[[1]]$output[[1]]$SSB))

names(mdf) <- c("Scenario", "Year","SSB","SD","lb","ub")
source("R/prelims.R")
ggplot(mdf,aes(x=Year,y=SSB,fill=Scenario,color=Scenario)) + xlim(2010,2026) + ylim(0,15000) + geom_line() + 
  geom_ribbon(aes(ymin=lb,ymax=ub),alpha=.3) + mytheme + geom_hline(yintercept=5500,linetype="dashed")+ geom_vline(xintercept=2020,linetype="dashed")

#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000

library(tidyverse)

FinMod$Model_1.0$output$Stock_1$SSB[,1:2]
FinMod$Model_1.0$output$Stock_1$R[,1:2]

mod_2stk <- readJJM("mod1.00.ls_2stk",path="config",input="input")

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

library(tidyverse)


Mod <- tibble(Year=modhl$Model_1.0$output$Stock_1$SSB[,1],
              High_Productivity=modhl$Model_1.0$output$Stock_1$SSB[,2],
              Low_Productivity=modls$Model_1.0$output$Stock_1$SSB[,2]) %>% 
        pivot_longer(-Year, names_to="Model",values_to="SSB") %>%
        mutate(Model=str_replace(Model,"_", " ")) %>%
        group_by(Model)

ggplot(Mod) + geom_line(mapping=aes(x=Year,y=SSB,colour=Model)) + facet_wrap(~Model,ncol=1,scales="free_y") +
          theme_minimal(base_size=16) +
          theme(legend.position="none")

#-------------------------------------------
# Plotting fishing mortality at age by fleet
#-------------------------------------------
library(tidyverse)
Fvars <- grep("F_age",names(FinMod$Model_1.0$output$Stock_1))
mat <- rep(NA, (1+ncol(FinMod$Model_1.0$output$Stock_1[[Fvars[1]]])))
names(mat) <- c("Year",paste0("F_",c(1:(ncol(FinMod$Model_1.0$output$Stock_1[[Fvars[1]]])-1))),"Fleet")
for(i in seq_along(Fvars)) {
  temp <- cbind(FinMod$Model_1.0$output$Stock_1[[Fvars[i]]],FinMod$Model_1.0$data$Fnames[i])
  mat <- rbind(mat,temp)
}
F_plot_1<- mat[-1,] %>% as_tibble() %>% 
            group_by(Year,Fleet) %>%
            pivot_longer(cols=starts_with("F_"),names_to="Age",values_to="Fishing_Mortality") %>%
            ungroup() %>%
            mutate(Year=as.numeric(Year),Age=as.numeric(str_remove(Age,"F_")),Fishing_Mortality=as.numeric(Fishing_Mortality)) %>% 
            filter(Year>2010) %>%
            group_by(Fleet,Year) %>%
            ggplot(mapping=aes(x=Age,y=Fishing_Mortality,fill=Fleet)) + 
              geom_area(position="fill") + facet_wrap(~Year,ncol=1,scales="free_y") + 
              theme_minimal(base_size=10) 
F_plot_1

F_plot_2<- mat[-1,] %>% as_tibble() %>% 
            group_by(Year,Fleet) %>%
            pivot_longer(cols=starts_with("F_"),names_to="Age",values_to="Fishing_Mortality") %>%
            ungroup() %>%
            mutate(Year=as.numeric(Year),Age=as.numeric(str_remove(Age,"F_")),Fishing_Mortality=as.numeric(Fishing_Mortality)) %>% 
            filter(Year>2010) %>%
            group_by(Fleet,Year) %>%
            ggplot(mapping=aes(x=Age,y=Fishing_Mortality,fill=Fleet)) + 
              geom_area() + facet_wrap(~Year,ncol=1,scales="free_y") + 
              theme_minimal(base_size=10) 
F_plot_2


#---------------
# Risk Tables
#------------

FinMod_msy <- modhl 
FinMod_msy <- modls 
FinMod_msy <- modll 
FinMod_msy <- modhs 
modls[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(modls, format="pdf", output="risk_tables/")

modls_adj[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(modls_adj, format="html", output="risk_tables/")
report(modls_adj, format="pdf", output="risk_tables/")

modls_2stk <- readJJM(paste0(modname,".ls","_2stk",path="config",input="input")
report(modls_2stk, format="pdf", output="risk_tables/")


report(modhs, format="pdf", output="risk_tables/")

modhl[[1]]$output[[1]]$msy_mt[,13] <- modhl[[1]]$output[[1]]$msy_mt[,12]/ 5500
report(modhl, format="pdf", output="risk_tables/")
modll[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(modll, format="pdf", output="risk_tables/")
modls[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(modls, format="pdf", output="risk_tables/")

report(modls, format="html", output="risk_tables/")

kobe(modls)
FinMod_msy <- fixed_bmsy(FinMod)
FinMod_msy


report(FinMod_msy, format="pdf", output="risk_tables/")
summary(FinMod_msy, Projections=TRUE, Fmult=1.54)

#--------------------------
# Tables for the Tech Annex
# Juan Carlos Quiroz's tables
# Loads packages
#--------------------------

source("R/constructor_input_tables.R")

construct_input_tables(modname="mod1.00",docname="Input_Tables_SC07")

# Summary data

write.csv(summary(mod1.00)$like,"results/SummaryLikelihoods.csv")


#--------------------------
# Generate summary table for historical retro
#--------------------------

final.modname <- mod1.00   # current year assessment
repfile <- final.modname[[1]]$output[[1]]
assessmenttype <- "update"

old.table <- read.csv("utils/SPRFMO historical retro.csv",header = T)
vars2pull <- colnames(old.table)
Nvars <- length(vars2pull)
Nyrs <- length(repfile$Yr)
sumtable <- matrix(NA, nrow = Nyrs, ncol = Nvars)
colnames(sumtable) <- vars2pull

old.table<-old.table[-which(old.table[,"Assessmentyear"]==max(repfile$Yr)),]

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

write.csv(as.data.frame(new.table), file = "utils/SPRFMO historical retro.csv", quote = FALSE,row.names = F)

#-------------------------------------------
# mod1.0_r: Retrospective analysis on mod1.0
# Code won't work unless go into jjmR directory
# and do devtools::load_all()
#-------------------------------------------
library(foreach)
library(doParallel)
registerDoParallel(5)

mod1.00 <- readJJM("mod1.00", path = "config", input = "input")
mod1.00r <- mod1.00
names(mod1.00r) <- "mod1.00"
Npeels<-5

 dir.jjmR <- "~/Google\ Drive/SPRFMO/jjmR"
 dir.jjm <- getwd()
 setwd(dir.jjmR)
 devtools::load_all()
 setwd(dir.jjm)
 ret1 <- retro(model = mod1.00r, n = Npeels, output = "results", exec="../src/jjms",parallel=T)

load("results/mod1.00_retrospective.RData")
ret1<-output
pdf("results/1.00Retro.pdf")
plot(ret1) # all plots
dev.off()

plot(ret1, var="SSB") # only SSB
#install.packages("icesAdvice")
#Calculation of Mohn's Rho courtesy of Arni Magnusson
icesAdvice::mohn(ret1$Stock_1$SSB$var[,1,1:6],peel=5,details=T)




