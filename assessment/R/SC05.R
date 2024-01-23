# ------------------------------------------------------------------------
# Script for SC05---------------------------------------------------------
# 2017 JM update----------------------------------------------------------
# ------------------------------------------------------------------------
#install.packages("devtools")
#library(devtools)
#devtools::install_github("sprfmo/jjmr")
R
library(jjmR)

# Set parameters ----------------------------------------------------------
# Path of JJM repository (from current working directory)
#setwd("/Users/leeqi/Google\ Drive/SPRFMO/jjm/assessment")
setwd("~/_mymods/sprfmo/jjm/assessment")

if (!grepl(basename(getwd()), "assessment", ignore.case = TRUE)) {
  stop(paste("Set working directory to assessment folder in jjm"))
}
#setwd("~/jjm/assessment")

# Running incremental data additions
modnames <- paste0("mod0.", 0:7)

library(foreach)
library(doParallel)

Ncores <- detectCores() - 2

cl <- makeCluster(Ncores)
registerDoParallel(cl)

foreach(i = 1:length(modnames)) %dopar% {
	#library(jjmR)
	modnames <- paste0("mod0.", 0:7)
	#setwd("/Users/leeqi/Google\ Drive/SPRFMO/jjm/assessment")
  #setwd("~/_mymods/sprfmo/jjm/assessment")
	runit(modnames[i],pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
}

stopCluster(cl)

#-----------------------------
# Data increments
#-----------------------------
mod0.0 <- readJJM("mod0.0", path = "config", input = "input")
mod0.1 <- readJJM("mod0.1", path = "config", input = "input")
mod0.2 <- readJJM("mod0.2", path = "config", input = "input")
mod0.3 <- readJJM("mod0.3", path = "config", input = "input")
mod0.4 <- readJJM("mod0.4", path = "config", input = "input")
mod0.5 <- readJJM("mod0.5", path = "config", input = "input")
mod0.6 <- readJJM("mod0.6", path = "config", input = "input")
mod0.7 <- readJJM("mod0.7", path = "config", input = "input")

mod0_7 <- combineModels(mod0.0,mod0.1, mod0.2, mod0.3, mod0.4, mod0.5, mod0.6 ,mod0.7)
png("mod0_recruitment.png")
plot(mod0_7,combine=T,what="recruitment",stack=F)
dev.off()
png("mod0_biomass.png")
plot(mod0_7,combine=T,what="biomass",stack=F)
dev.off()

pdf("mod0_plots.pdf")
plot(mod0_7,combine=T,stack=F,what = "all")
dev.off()
plot(mod0_7,combine=T,what="biomass",stack=F)

dev.off()

#-------------------------------
# Model runs
#-------------------------------
modname <- "mod1.0"
#mod1.0 <- runit(modname, pdf = TRUE, portrait = F, est = TRUE, exec = "../src/jjms")
mod1.0 <- readJJM(modname, path = "config", input = "input")

#modname <- "mod1.1"
#mod1.1 <- runit(modname, pdf = TRUE, portrait = F, est = TRUE, exec = "../src/jjms")
#
#modname <- "mod1.2"
#mod1.2 <- runit(modname, pdf = TRUE, portrait = F, est = TRUE, exec = "../src/jjms")

mod1.0_msy <- mod1.0
mod1.0_msy[[1]]$output[[1]]$msy_mt[,10] <- 5500

report(mod1.0_msy, format="pdf", output="risk_tables/")

mod1_1 <-  combineModels(mod0.0, mod1.0)
pdf("mod1_1_plots.pdf")
plot(mod1_1,combine=T,what="biomass",stack=F)
plot(mod1_1,combine=T,what="recruitment",stack=F)
dev.off()

#--------------------------
# Kobe plot
# Combining 2016 model with 2017
#--------------------------

k1=jjmR:::.kobeFUN(mod1.0[[1]]$output$Stock_1, xlim=c(0,5), ylim=c(0,8))
png("Kobe.png")
plot(k1)
jjmR:::.kobeFUN(mod0.0[[1]]$output$Stock_1, pic.add = k1, add = T, xlim=c(0,5), ylim=c(0,8), col = rgb(0,0,0,0.3))
dev.off()

#--------------------------
# Figures for the Tech Annex
# Writes plots to /annex plots
# Code needs to be edited for 
# two-stock model and 
# likelihood profiles
#--------------------------
dir.main <- getwd()
dir.plots <- "annex plots"
dir.create(dir.plots)

modnm <- "mod1.0"

# Width and height in pixels for most graphs
# The rest will have to changed manually
plotsizes <- c(750, 500)

source("AnnexPlots.R")


#--------------------------
# Historic advice table
# Martin Pastoors' graphs
# Writes summary table to
# utils/SPRFMO historical retro.csv
# graph to 
#--------------------------
library(tidyverse)

final.modname <- mod1.0 # current year assessment
repfile <- final.modname[[1]]$output[[1]]

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

if(max(old.table$Assessmentyear)>=max(repfile$Yr)) {
	temp <- old.table[-which(old.table$Assessmentyear == max(old.table$Assessmentyear)),]
	new.table <- rbind(temp, sumtable)
}

if(max(old.table$Assessmentyear)<max(repfile$Yr)) {
	new.table <- rbind(old.table, sumtable)
}

write.csv(as.data.frame(new.table), file = "utils/SPRFMO historical retro.csv", quote = FALSE,row.names = F)

cjm <- as.tibble(new.table) %>%
  setNames(., tolower(colnames(.)))

source("historic retro.r")
#colnames(msy_mt) == Yr Fspr 1-Fspr F/Fmsy Fmsy F Fsprmsy MSY MSYL Bmsy Bzero B/Bmsy


#--------------------------
# Tables for the Tech Annex
# Juan Carlos Quiroz's tables
# Cleans out workspace!
#--------------------------

source("constructor_input_tables.R")


