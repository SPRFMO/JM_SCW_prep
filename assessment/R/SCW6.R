# ------------------------------------------------------------------------
# Script for SCW6---------------------------------------------------------
# 2018 JM benchy ----------------------------------------------------------
# ------------------------------------------------------------------------
#install.packages("devtools")
#
# MAKE SURE TO UPDATE
#devtools::install_github("sprfmo/jjmr")

library(jjmR)

#--------------------------------------------------------
# Working directory should be in assessment folder of jjm
#--------------------------------------------------------

if (!grepl(basename(getwd()), "assessment", ignore.case = TRUE)) {
  stop(paste("Set working directory to assessment folder in jjm"))
}

#-------------------------
# Model configuration runs
#-------------------------
# mod1.0 <- runit("mod1.0",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.1 <- runit("mod1.1",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.2 <- runit("mod1.2",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.3 <- runit("mod1.3",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.4 <- runit("mod1.4",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.5 <- runit("mod1.5",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.6 <- runit("mod1.6",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.7 <- runit("mod1.7",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.8 <- runit("mod1.8",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.9 <- runit("mod1.9",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.10<- runit("mod1.10",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.11<- runit("mod1.11",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.12<- runit("mod1.12",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.13<- runit("mod1.13",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.14<- runit("mod1.14",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.15<- runit("mod1.15",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# mod1.16<- runit("mod1.16",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

mod1.0 <- readJJM("mod1.0", path = "config", input = "input")
mod1.1 <- readJJM("mod1.1", path = "config", input = "input")
mod1.2 <- readJJM("mod1.2", path = "config", input = "input")
mod1.3 <- readJJM("mod1.3", path = "config", input = "input")
mod1.4 <- readJJM("mod1.4", path = "config", input = "input")
mod1.5 <- readJJM("mod1.5", path = "config", input = "input")
mod1.6 <- readJJM("mod1.6", path = "config", input = "input")
mod1.7 <- readJJM("mod1.7", path = "config", input = "input")
mod1.8 <- readJJM("mod1.8", path = "config", input = "input")
mod1.9 <- readJJM("mod1.9", path = "config", input = "input")
mod1.10<- readJJM("mod1.10", path = "config", input = "input")
mod1.11<- readJJM("mod1.11", path = "config", input = "input")
mod1.12<- readJJM("mod1.12", path = "config", input = "input")
mod1.13<- readJJM("mod1.13", path = "config", input = "input")
mod1.14<- readJJM("mod1.14", path = "config", input = "input")
mod1.15<- readJJM("mod1.15", path = "config", input = "input")
mod1.16<- readJJM("mod1.16", path = "config", input = "input")

mod2018W <- readJJM("model2018w", path = "config", input = "input")
mod2017  <- readJJM("model2017", path = "config", input = "input")

AllMods <- combineModels(mod1.0,mod1.1,mod1.2,mod1.3,mod1.4,mod1.5,mod1.6,mod1.7,mod1.8,mod1.9,mod1.10,mod1.11,mod1.12,mod1.13,mod1.14,mod1.15,mod1.16)

#-----------------------------------------
# Comparing impact of fixing selectivities
#-----------------------------------------
mod0_4 <- combineModels(mod1.0,mod1.1, mod1.2, mod1.3, mod1.4)
pdf("results/combine0_4.pdf")
plot(mod0_4,combine=T,what="recruitment",stack=F)
plot(mod0_4,combine=T,what="biomass",stack=F)
plot(mod0_4,combine=T,what="ftot",stack=F)
dev.off()

#----------------------------------------------------
# Comparing effects of different indices combinations
#----------------------------------------------------
mod5_10 <- combineModels(mod1.0,mod1.5,mod1.6,mod1.7,mod1.8,mod1.9,mod1.10)
pdf("results/combine5_10.pdf")
plot(mod5_10,combine=T,what="recruitment",stack=F)
plot(mod5_10,combine=T,what="biomass",stack=F)
plot(mod5_10,combine=T,what="ftot",stack=F)
dev.off()

#----------------------------------------------------
# Comparing 2017, 2018W and 1.4 and 1.5
#----------------------------------------------------
mod2017_108W_4_5 <- combineModels(mod2017, mod2018W, mod1.4,mod1.5)
pdf("results/combine2017_2018W_1.4_1.5.pdf")
plot(mod2017_108W_4_5,combine=T,what="recruitment",stack=F)
plot(mod2017_108W_4_5,combine=T,what="biomass",stack=F)
plot(mod2017_108W_4_5,combine=T,what="ftot",stack=F)
dev.off()


#-------------------------------------------
# mod1.5_r: Retrospective analysis on mod1.5
#-------------------------------------------
mod1.5r <- mod1.5
names(mod1.5r) <- "mod1.5"
Npeels<-10
# ret1 <- retro(model = mod1.5r, n = Npeels, output = "results", exec="../src/jjms",parallel=T)
# ret2 <- retro(model = mod1.4r, n = Npeels, output = "results", exec="../src/jjms",parallel=T)
# ret3 <- retro(model = mod1.13r, n = Npeels, output = "results", exec="../src/jjms",parallel=T)

load("results/mod1.5_retrospective.RData")
ret1<-output
load("results/mod1.4_retrospective.RData")
ret2 <- output
load("results/mod1.13_retrospective.RData")
ret3 <- output

pdf("results/combine_1.5r.pdf")
plot(ret1) # all plots
dev.off()

pdf("results/combine_1.4r.pdf")
plot(ret2) # all plots
dev.off()

pdf("results/combine_1.13r.pdf")
plot(ret3) # all plots
dev.off()

plot(ret1, var="SSB") # only SSB
plot(ret2, var="SSB", std=TRUE) # Relative bias
#install.packages("icesAdvice")
#Calculation of Mohn's Rho courtesy of Arni Magnusson
icesAdvice::mohn(ret1$Stock_1$SSB$var[,1,1:6],peel=5,details=T)
icesAdvice::mohn(ret3$Stock_1$SSB$var[,1,1:6],peel=5,details=T)

wtsLines <- grep("FW",names(mod1.5[[1]]$output$Stock_1))
Nwts <- length(wtsLines)
#FW <- matrix(nrow=Npeels,ncol=Nwts)
#colnames(FW) <- names(mod0_4[[1]]$output$Stock_1[wtsLines])
for(i in 1:Npeels) {
	if(i<10) {j<-paste0("0",i)}
	else{j<-i}
	repname<-paste0("results/mod1.5_r",j,"_1_R.rep")
	temp<-readLines(repname)
	wtsLines<-wtsLines <- grep("FW",temp)+1
	FW[cnt,2:(Nwts+1)] <- as.numeric(temp[wtsLines])
	FW[cnt,1] <- paste0("mod1.5_r_",j)
	cnt<-cnt+1
}

#--------------------------------------------------
# Allowing more flexibility in fishery selectivity
#--------------------------------------------------
mod5_11 <- combineModels(mod1.5, mod1.11)
pdf("results/combine5_11.pdf")
plot(mod5_11,combine=T,what="recruitment",stack=F)
plot(mod5_11,combine=T,what="biomass",stack=F)
plot(mod5_11,combine=T,what="ftot",stack=F)
dev.off()

#------------------------------
# Changing growth relationships
#------------------------------
mod12_14 <- combineModels(mod1.5, mod1.12, mod1.14) #Add 1.16 later
pdf("results/combine12_14.pdf")
plot(mod12_14,combine=T,what="recruitment",stack=F)
plot(mod12_14,combine=T,what="biomass",stack=F)
plot(mod12_14,combine=T,what="ftot",stack=F)
dev.off()

#--------------------------------------------------
# Reweighting age compositions with Francis Weights
#--------------------------------------------------
mod5_13 <- combineModels(mod1.5, mod1.13)
pdf("results/combine5_13.pdf")
plot(mod5_13,combine=T,what="recruitment",stack=F)
plot(mod5_13,combine=T,what="biomass",stack=F)
plot(mod5_13,combine=T,what="ftot",stack=F)
dev.off()

#-------------------------------------
# Catchability Change for Chilean CPUE
#-------------------------------------
mod5_15 <- combineModels(mod1.5, mod1.15)
pdf("results/combine5_15.pdf")
plot(mod5_15,combine=T,what="recruitment",stack=F)
plot(mod5_15,combine=T,what="biomass",stack=F)
plot(mod5_15,combine=T,what="ftot",stack=F)

dev.off()

#----------------------------------------------------
# As 1.11 but downweight age comps from 2016 and 2017
#----------------------------------------------------
mod1.11_a<-runit("mod1.11_a",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
mod1.11_a<-readJJM("mod1.16", path = "config", input = "input")
mod11_11a<-combineModels(mod1.11, mod1.11_a)

#-------------------------------------
# Likelihood and Francis Weight Tables
#-------------------------------------

write.csv(summary(AllMods)$like,"results/SummaryLikelihoods.csv")
names(summary(AllMods))

names(AllMods)
FWelements<-grep("FW", names(AllMods[[1]]$output$Stock_1))
Nmodels<- length(names(AllMods))
FW <- rep(1, length(FWelements))
for(i in 1:length(names(AllMods))) {
	FW<- rbind(FW,AllMods[[i]]$output$Stock_1[FWelements])
}
FW <- FW[-1,]
rownames(FW) <- names(AllMods)
write.csv(FW,"results/FrancisWeights.csv")

#-------------------------------
# Risk Table
# Not sure if this works
#-------------------------------
mod1.13_msy <- mod1.13
mod1.13_msy[[1]]$output[[1]]$msy_mt[,10] <- 5500

report(mod1.0_msy, format="pdf", output="risk_tables/")

mod1_1 <-  combineModels(mod0.0, mod1.0)
pdf("mod1_1_plots.pdf")
plot(mod1_1,combine=T,what="biomass",stack=F)
plot(mod1_1,combine=T,what="recruitment",stack=F)
dev.off()

# Generate summary table for historical retro


final.modname <- mod2018W # current year assessment
repfile <- final.modname[[1]]$output[[1]]
assessmenttype <- "benchmark"

old.table <- read.csv("utils/SPRFMO historical retro.csv",header = T)
vars2pull <- colnames(old.table)
Nvars <- length(vars2pull)
Nyrs <- length(repfile$Yr)
sumtable <- matrix(NA, nrow = Nyrs, ncol = Nvars)
colnames(sumtable) <- vars2pull

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

