# ------------------------------------------------------------------------
# Script for SC06---------------------------------------------------------
# 2018 JM  ---------------------------------------------------------------
# ------------------------------------------------------------------------
#install.packages("devtools")
#
# MAKE SURE TO UPDATE
devtools::install_github("sprfmo/jjmr")

library(jjmR)

#--------------------------------------------------------
# Working directory should be in assessment folder of jjm
#--------------------------------------------------------

setwd(file.path(getwd(), "assessment"))

if (!grepl(basename(getwd()), "assessment", ignore.case = TRUE)) {
  stop(paste("Set working directory to assessment folder in jjm"))
}
source("R/rename.R")

#-------------------------
# Model configuration runs
#-------------------------

# Check models are the same

#mod0.00 <- runit("mod0.00",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
##mod2018 <- runit("model2018w",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
#mod0.00 <- readJJM("mod0.00", path = "config", input = "input")
#mod2018w <- readJJM("model2018w", path = "config", input = "input")
#
#oldnewMods <- combineModels(mod0.00,mod2018w)
plot(recmods,combine=T,stack=F)
#plot(oldnewMods,combine=T,what="biomass",stack=F)
#plot(oldnewMods,combine=T,what="ftot",stack=F)

#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# Projection runs
#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
modhl  <- runit("mod1.4.hl",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
modll  <- runit("mod1.4.ll",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
modhs  <- runit("mod1.4.hs",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
modls  <- runit("mod1.4.ls",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod1.5 <- runit("mod1.5",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

modhl <- readJJM("mod1.4.hl",path="config",input="input")
modll <- readJJM("mod1.4.ll",path="config",input="input")
modhs <- readJJM("mod1.4.hs",path="config",input="input")
modls <- readJJM("mod1.4.ls",path="config",input="input")
recmods <- combineModels(modhl,modhs,modll,modls)
recmods <- changeNameModel(recmods,c( "h=0.8, full series","h=0.8, short series","h=0.65, full series","h=0.65, short series" ))
# Function to put a constant reference point into image
fixed_bmsy <- function(mod,refpt=5500){
	old_rat <- (mod[[1]]$output[[1]]$msy_mt[,13])
  new_rat <- (mod[[1]]$output[[1]]$msy_mt[,12]/ refpt)
  mod[[1]]$output[[1]]$msy_mt[,13] <- new_rat
  return(mod)
}
kobe( fixed_bmsy(modls) )
kobe( fixed_bmsy(modhl) ,add=T) 
kobe( fixed_bmsy(modls) ,add=T)
kobe( fixed_bmsy(modhs) ,add=T)

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
names(mdf) <- c("Scenario", "Year","SSB","SD","lb","ub")
library(ggplot2)
source("R/prelims.R")
ggplot(mdf,aes(x=Year,y=SSB,fill=Scenario,color=Scenario)) + xlim(2010,2025) + ylim(0,15000)+ geom_line() + 
                   geom_ribbon(aes(ymin=lb,ymax=ub),alpha=.3,) + mytheme + geom_hline(yintercept=5000,linetype="dashed")+ geom_vline(xintercept=2018,linetype="dashed")

#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000



#----------------
# Data increments
#----------------
# mod0.01 <- runit("mod0.01",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.02 <- runit("mod0.02",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.03 <- runit("mod0.03",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.04 <- runit("mod0.04",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.05 <- runit("mod0.05",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.06 <- runit("mod0.06",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.07 <- runit("mod0.07",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.08 <- runit("mod0.08",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.09 <- runit("mod0.09",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.10<- runit("mod0.10",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.11<- runit("mod0.11",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod0.12<- runit("mod0.12",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

mod0.00 <- readJJM("mod0.00", path = "config", input = "input")
mod0.01 <- readJJM("mod0.01", path = "config", input = "input")
mod0.02 <- readJJM("mod0.02", path = "config", input = "input")
mod0.03 <- readJJM("mod0.03", path = "config", input = "input")
mod0.04 <- readJJM("mod0.04", path = "config", input = "input")
mod0.05 <- readJJM("mod0.05", path = "config", input = "input")
mod0.06 <- readJJM("mod0.06", path = "config", input = "input")
mod0.07 <- readJJM("mod0.07", path = "config", input = "input")
mod0.08 <- readJJM("mod0.08", path = "config", input = "input")
mod0.09 <- readJJM("mod0.09", path = "config", input = "input")
mod0.10 <- readJJM("mod0.10", path = "config", input = "input")
mod0.11 <- readJJM("mod0.11", path = "config", input = "input")
mod0.12 <- readJJM("mod0.12", path = "config", input = "input")

CatchMods <- combineModels(mod0.01,mod0.00)
plot(CatchMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(CatchMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(CatchMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")

CPUEMods <- combineModels(mod0.00,mod0.01,mod0.02,mod0.03,mod0.04,mod0.05,mod0.06)
plot(CPUEMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(CPUEMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(CPUEMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")

CompMods <- combineModels(mod0.06,mod0.07,mod0.08,mod0.09,mod0.10,mod0.11,mod0.12)
plot(CompMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(CompMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(CompMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")

CompMods <- combineModels(mod0.11,mod0.12)
plot(CompMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(CompMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(CompMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")

AllDatMods <- combineModels(mod0.00,mod0.01,mod0.02,mod0.03,mod0.04,mod0.05,mod0.06,mod0.07,mod0.08,mod0.09,mod0.10,mod0.11,mod0.12)
plot(AllDatMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(AllDatMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(AllDatMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")

FinalDatMods <- combineModels(mod0.00,mod0.12)
dims <- c(7,9)
pdf(file.path("results", "UpdatedDat.pdf"), height = dims[1],width = dims[2])
plot(FinalDatMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(FinalDatMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(FinalDatMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")
dev.off()

#--------------------
# Model sensitivities
#--------------------
# mod1.0 <- runit("mod1.0",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.1 <- runit("mod1.1",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.2 <- runit("mod1.2",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.3 <- runit("mod1.3",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

mod1.0 <- readJJM("mod1.0", path = "config", input = "input")
mod1.1 <- readJJM("mod1.1", path = "config", input = "input")
mod1.2 <- readJJM("mod1.2", path = "config", input = "input")
mod1.3 <- readJJM("mod1.3", path = "config", input = "input")
mod1.4 <- readJJM("mod1.4", path = "config", input = "input")
mod1.5 <- readJJM("mod1.5", path = "config", input = "input")

CompMods <- combineModels(mod1.0,mod1.1,mod1.2,mod1.3,mod1.4,mod1.5)
plot(CompMods,combine=T,what="biomass",stack=F,main="Biomass")


#---------------------------------
# Downweighting Chinese CPUE Index
#---------------------------------
ChinaCPUEMods <- combineModels(mod1.0,mod1.1)
pdf(file.path("results", "ChinaCPUE.pdf"), height = dims[1],
            width = dims[2])
plot(ChinaCPUEMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(ChinaCPUEMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(ChinaCPUEMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")
dev.off()

#---------------------------------
# Replacing Peruvian CPUE Index with old (May 2018) index
#---------------------------------
PeruCPUEMods <- combineModels(mod1.0,mod1.2)
pdf(file.path("results", "PeruCPUE.pdf"), height = dims[1],
            width = dims[2])
plot(PeruCPUEMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(PeruCPUEMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(PeruCPUEMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")
dev.off()

#---------------------------------
# Changing growth relationship
#---------------------------------
GrowthMods <- combineModels(mod1.0,mod1.3)
pdf(file.path("results", "Growth.pdf"), height = dims[1],
            width = dims[2])
plot(GrowthMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(GrowthMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(GrowthMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")
dev.off()

#--------------------------------------------------
# Reweighting age compositions with Francis Weights
#--------------------------------------------------
mod1.0 <- readJJM("mod1.0", path = "config", input = "input")

FWelements<-grep("FW", names(mod1.0[[1]]$output$Stock_1))
Nmodels<- length(names(mod1.0))
FW <- rep(1, length(FWelements))
for(i in 1:length(names(mod1.0))) {
	FW<- rbind(FW,mod1.0[[i]]$output$Stock_1[FWelements])
}
FW <- FW[-1,]
FW
#write.csv(FW,"results/FrancisWeights.csv")

# mod1.4 <- runit("mod1.4",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod1.4 <- readJJM("mod1.4", path = "config", input = "input")

FWMods <- combineModels(mod1.0,mod1.4)
dims <- c(7,9)
pdf(file.path("results", "FrancisWeights.pdf"), height = dims[1],
            width = dims[2])
plot(FWMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(FWMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(FWMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")
dev.off()

#-----------------------------
# Compare previous assessments
#-----------------------------
dims <- c(7,9)
#mod2018w <- runit("model2018w",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
#mod2017 <- runit("model2017",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

mod2018 <- readJJM("mod1.4", path = "config", input = "input")
mod2018w <- readJJM("model2018w", path = "config", input = "input")
mod2017 <- readJJM("model2017", path = "config", input = "input")

prevMods <- combineModels(mod2018,mod2018w,mod2017)
pdf(file.path("results", "PrevMods.pdf"), height = dims[1],
            width = dims[2])
plot(prevMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(prevMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(prevMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")
dev.off()


#-------------------------------------------
# mod1.4_r: Retrospective analysis on mod1.4
# Code won't work unless go into jjmR directory
# and do devtools::load_all()
#-------------------------------------------
library(foreach)
library(doParallel)
registerDoParallel(5)

mod1.4 <- readJJM("mod1.4", path = "config", input = "input")
mod1.4r <- mod1.4
names(mod1.4r) <- "mod1.4"
Npeels<-5

# ret1 <- retro(model = mod1.4r, n = Npeels, output = "results", exec="../src/jjms",parallel=T)

load("results/mod1.4_retrospective.RData")
ret1<-output
pdf("results/1.4Retro.pdf")
plot(ret1) # all plots
dev.off()

plot(ret1, var="SSB") # only SSB
#install.packages("icesAdvice")
#Calculation of Mohn's Rho courtesy of Arni Magnusson
icesAdvice::mohn(ret1$Stock_1$SSB$var[,1,1:6],peel=5,details=T)

wtsLines <- grep("FW",names(mod1.4r[[1]]$output$Stock_1))
Nwts <- length( wtsLines)
FW <- data.frame(mod1.4r[[1]]$output$Stock_1[wtsLines])
colnames(FW) <- unlist(strsplit(colnames(FW),split="FW_"))[seq(2,(Nwts*2),by=2)]
cnt<-1
for(i in 1:Npeels) {
	if(i<10) {j<-paste0("0",i)}
	else{j<-i}
	repname<-paste0("results/mod1.4_r",j,"_1_R.rep")
	temp<-readLines(repname)
	wtsLines<-wtsLines <- grep("FW",temp)+1
	FW[cnt,] <- as.numeric(temp[wtsLines])
#	FW[cnt,1] <- paste0("mod1.4r_",j)
	cnt<-cnt+1
}


FWtidy <- tidyr::gather(FW,key="DataSource",value="Weights")
ggplot2::ggplot(FWtidy,mapping=ggplot2::aes(x=DataSource,y=Weights)) +
	ggplot2::geom_boxplot() +
	ggplot2::theme_minimal() +
	ggplot2::theme(text = ggplot2::element_text(size = 20))


#--------------------------------------------
# Last year's model 1.0 with this year's data
#--------------------------------------------
#model2017 <- runit("mod2017_newdat",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
#model2018w <- runit("mod0.11",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
#model2018 <- runit("mod1.0",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
#model2017r <- runit("mod2017_newdat",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

mod2017 <- readJJM("mod2017_newdat", path = "config", input = "input")
mod2018w<- readJJM("mod0.11", path = "config", input = "input")
mod2018 <- readJJM("mod1.0", path = "config", input = "input")

dims <- c(7,9)
CompMods <- combineModels(mod2018,mod2018w,mod2017)
pdf(file.path("results", "PrevMods.pdf"), height = dims[1],
            width = dims[2])
plot(CompMods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(CompMods,combine=T,what="biomass",stack=F,main="Biomass")
plot(CompMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")
dev.off()

#model2017r <- readJJM("mod2017_newdat",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
#CompMods <- combineModels(mod1.0,mod2017,model2017r)
#plot(CompMods,combine=T,what="recruitment",stack=F,main="Recruitment")
#plot(CompMods,combine=T,what="biomass",stack=F,main="Biomass")
#plot(CompMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")

#---------------
# More weighting
# Unofficial testing
# Upweighted age comps
#---------------
#mod1.5 <- runit("mod1.5",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
#library(jjmR)
#mod1.5 <- readJJM("mod1.5", path = "config", input = "input")
#mod1.4 <- readJJM("mod1.4", path = "config", input = "input")
#mod1.0 <- readJJM("mod1.0", path = "config", input = "input")
#
#WeightMods <- combineModels(mod1.0,mod1.4,mod1.5)
#plot(WeightMods,combine=T,what="recruitment",stack=F,main="Recruitment")
#plot(WeightMods,combine=T,what="biomass",stack=F,main="Biomass")
#plot(WeightMods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")


#---------------
# Risk Tables
#------------
 exp(9.588)
 9582.1
 log(9582.1)-(9.588)  


mod1.4_msy <- modhl 
mod1.4_msy <- modls 
mod1.4_msy <- modll 
mod1.4_msy <- mod1.4 
mod1.5_msy <- mod1.5 
# Low steepness, recent recruitment
mod1.5_msy[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(mod1.5_msy, format="pdf", output="risk_tables/")
# High steepness, all recruitment
mod1.4_msy[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(mod1.4_msy, format="pdf", output="risk_tables/")

modhs[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(modhs, format="pdf", output="risk_tables/")

modhl[[1]]$output[[1]]$msy_mt[,13] <- modhl[[1]]$output[[1]]$msy_mt[,12]/ 5500
report(modhl, format="pdf", output="risk_tables/")
modll[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(modll, format="pdf", output="risk_tables/")
modls[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(modls, format="pdf", output="risk_tables/")
(modls[[1]]$output$Stock_1$SSB)
(modll[[1]]$output$Stock_1$SSB)
(modhl[[1]]$output$Stock_1$SSB)
(modhs[[1]]$output$Stock_1$SSB)

kobe(modls)
mod1.4_msy <- mod1.4
mod1.4_msy[[1]]$output[[1]]$msy_mt[,10] <- 5500

report(mod1.4_msy, format="pdf", output="risk_tables/")

mod1.3_msy <- mod1.3
mod1.3_msy[[1]]$output[[1]]$msy_mt[,10] <- 5500

report(mod1.3_msy, format="pdf", output="risk_tables/")


#--------------------------
# Tables for the Tech Annex
# Juan Carlos Quiroz's tables
# Loads packages
#--------------------------

source("R/constructor_input_tables.R")

construct_input_tables(modname="mod1.4",docname="Input_Tables_SC06")

# Summary data

write.csv(summary(mod1.4)$like,"results/SummaryLikelihoods.csv")




