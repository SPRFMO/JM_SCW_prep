# Code to make figures for CJM technical annex
# Based on technical annex for SC03 and discussion with M. Pastoors
# Saves plots to png files in "annex plot" folder
# leeqi@uw.edu

#-----------------
# Loading packages
#-----------------
#
#if(!require(devtools)) {install.packages("devtools")}
#if(!require(jjmR)) {
#	library(devtools)
#	devtools::install_github("sprfmo/jjmr")
#}
library(jjmR)
#
#----------------------
# User-specified inputs
#----------------------

dir.main <- getwd()
dir.plots <- "annex plots"
if(!dir.exists(dir.plots)) dir.create(dir.plots)

h1nm <- "h1_1.05"

h2nm <- "h2_1.05"

# Width and height in pixels for most graphs
# The rest will have to changed manually
plotsizes <- c(10, 7)

#--------------------------------
# Reading in and combining models
#--------------------------------
h1.mod <- readJJM(h1nm, path = "config", input = "input")
h1.diag <- diagnostics(h1.mod,plot=F)

h2.mod <- readJJM(h2nm,path = "config", input = "input")
h2.diag <- diagnostics(h2.mod, plots=F)

dev.off()
#---------------
# Start plotting
#---------------
plotCount <- 0

######
# Data
######
# Catch by fleet
plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h1nm, "_", "totalCatchByFleet", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h1.diag, var = "totalCatchByFleet")
dev.off()

# Meta data overview
plotCount <- plotCount+1
source("R/plot_metadataoverview.R")
pdf(paste0("annex plots/Fig", plotCount,"_metadata.pdf"),height=10,width=10)
print(p1)
dev.off()

## Retro
load(file = paste0("results/", h1nm, "_retrospective", ".RData"))
h1.retro <- output

load(file = paste0("results/", h2nm, "_retrospective", ".RData"))
h2.retro <- output

plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h1nm, "a_", "Retro_SSB", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h1.retro, var = "SSB")
dev.off()

png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h1nm, "b_", "Retro_Rec", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h1.retro, var = "R")
dev.off()

plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h2nm, "a_%02d_", "Retro_SSB", ".png")),
	width = plotsizes[1], height = plotsizes[2], units="in",res=300)
plot(h2.retro, var = "SSB")
dev.off()

png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h2nm, "b_%02d_", "Retro_Rec", ".png")),
	width = plotsizes[1], height = plotsizes[2], units="in",res=300)
plot(h2.retro, var = "R")
dev.off()

# Historical retrospective
plotCount <- plotCount + 1
source("R/plot_historicretro.R")
pdf(paste0("annex plots/Fig", plotCount,"_HistoricRetro.pdf"),height=10,width=7)
print(pg)
dev.off()

plotCount <- plotCount+1
pdf(paste0("annex plots/Fig", plotCount,"_HistoricRetro2.pdf"),height=10,width=10)
print(pg2)
dev.off()

# Fishery weight at age
plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"a_", h1nm, "_", "weightFishery", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h1.diag, var = "weightFishery")
dev.off()

# Survey weight at age
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"b_", h1nm, "_", "weightSurvey", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h1.diag, var = "weightAge")
dev.off()

## SSB and Rec comparing models
#plotCount <- plotCount + 1
#png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h1nm, "a_", "SSB", ".png")),
#	width = plotsizes[1], height = plotsizes[2]),units="in",res=300
#plot(mods.comb, what = "biomass", combine = TRUE, stack = FALSE)
#dev.off()
#
#png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h1nm, "b_", "Rec", ".png")),
#	width = plotsizes[1], height = plotsizes[2]),units="in",res=300
#plot(mods.comb, what = "recruitment", combine = TRUE, stack = FALSE)
#dev.off()

###
# Model fit to age/length comps
###
plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_%02d_", h1nm, "_", "AgeCompCatchFits", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h1.diag, var = "ageFitsCatch")
dev.off()

plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount, "_%02d_", h2nm, "_", "AgeCompCatchFits", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h2.diag, var = "ageFitsCatch")
dev.off()

plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_%02d_",h1nm, "_",  "LengthCompCatchFits", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h1.diag, var = "lengthFitsCatch")
dev.off()

plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount, "_%02d_", h2nm, "_", "LengthCompCatchFits", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h2.diag, var = "lengthFitsCatch")
dev.off()

plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_%02d_", h1nm, "_",  "AgeCompSurveyFits", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h1.diag, var = "ageFitsSurvey")
dev.off()

plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount, "_%02d_", h2nm, "_", "AgeCompSurveyFits", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h2.diag, var = "ageFitsSurvey")
dev.off()

# Predicted vs observed indices
plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h1nm, "_", "IndexFits", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h1.diag, var = "predictedObservedIndices")
dev.off()

plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount, "_%02d_", h2nm, "_", "IndexFits", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h2.diag, var = "predictedObservedIndices")
dev.off()

# Fishery mean age
plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h1nm, "_", "FishMeanAge", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h1.diag, var = "fisheryMeanAge")
dev.off()

plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount, "_%02d_", h2nm, "_", "FishMeanAge", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h2.diag, var = "fisheryMeanAge")
dev.off()

# Survey mean age
plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h1nm, "_", "SurveyMeanAge", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h1.diag, var = "surveyMeanAge")
dev.off()

plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount, "_%02d_", h2nm, "_", "SurveyMeanAge", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h2.diag, var = "surveyMeanAge")
dev.off()

# Survey mean length
plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h1nm, "_", "FishMeanLength", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h1.diag, var = "fisheryMeanLength")
dev.off()

plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h2nm, "_%02d_", "FishMeanLength", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h2.diag, var = "fisheryMeanLength")
dev.off()

# Selectivity estimates by fishery
plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h1nm, "_", "FishSelectivity", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h1.diag, var = "selectivityFisheryByPentad")
dev.off()

plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h2nm, "_%02d_", "FishSelectivity", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h2.diag, var = "selectivityFisheryByPentad")
dev.off()

# Selectivity estimates by survey
plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h1nm, "_", "SurvSelectivity", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h1.diag, var = "selectivitySurveyByPentad")
dev.off()

plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h2nm, "_%02d_", "SurvSelectivity", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h2.diag, var = "selectivitySurveyByPentad")
dev.off()

# Summary of stock status
plotCount <- plotCount + 1
fixed_bmsy <- function(mod,refpt=5500){
  old_rat <- (mod[[1]]$output[[1]]$msy_mt[,13])
  new_rat <- (mod[[1]]$output[[1]]$msy_mt[,12]/ refpt)
  mod[[1]]$output[[1]]$msy_mt[,13] <- new_rat
  mod[[1]]$output[[1]]$msy_mt[,10] <- refpt
  return(mod)
}
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h1nm, "_", "Summary", ".png")),
	width = plotsizes[2], height = plotsizes[2],units="in",res=300)
bmsy.diag <- diagnostics(fixed_bmsy(h1.mod), plots = F)
plot(bmsy.diag, var = "summarySheet")
dev.off()

plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h2nm, "_%02d_", "Summary", ".png")),
	width = plotsizes[2], height = plotsizes[2],units="in",res=300)
plot(h2.diag, var = "summarySheet")
dev.off()

# Unfished Biomass
plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h1nm, "_", "fishedUnfishedBiomass", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h1.diag, var = "fishedUnfishedBiomass")
dev.off()

plotCount <- plotCount + 1
png(file = file.path(dir.main, dir.plots, paste0("Fig", plotCount,"_", h2nm, "_%02d_", "fishedUnfishedBiomass", ".png")),
	width = plotsizes[1], height = plotsizes[2],units="in",res=300)
plot(h2.diag, var = "fishedUnfishedBiomass")
dev.off()


# To have in future?
#
## Heat map for age/length comps?
#
## Ratio of fish vs unfished biomass?
#


