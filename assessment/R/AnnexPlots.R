# Code to make figures for CJM technical annex
# Based on technical annex for SC03 and discussion with M. Pastoors
# Saves plots to PNG files in "annex plot" folder
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

modnm <- "mod1.00"

mod2stk <- "mod1.00_2stk"

# Width and height in pixels for most graphs
# The rest will have to changed manually
plotsizes <- c(750, 500)

# Number of recruitment values likelihood profile was done over
#Nrecs <- 11

# Parameter over which likelihood profile was done over
#par2change <- "N_Mort"
#Nreps <- 11

#--------------------------------
# Reading in and combining models
#--------------------------------
mainmod <- readJJM(modnm, path = "config", input = "input")
main.diag <- diagnostics(mainmod, plots = F)

mods.read <- list()
mods.read[[1]] <- mainmod

## If comparing multiple model configurations ---------
#
#for(i in 1:length(mods2compare)) {
#	mods.read[[i+1]] <- readJJM(mods2compare[i], path = "config", input = "input")
#}
#
#mods.comb <- do.call(combineModels, args = mods.read)
#
mod.2stk <- readJJM(mod2stk, path = "config", input = "input")
diag.2stk <- diagnostics(mod.2stk, plots=F)

#---------------
# Start plotting
#---------------
plotCount <- 0

# Catch by fleet
plotCount <- plotCount + 1
png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_", "totalCatchByFleet", ".png")),
	width = plotsizes[1], height = plotsizes[2])
plot(main.diag, var = "totalCatchByFleet")
dev.off()

# Fishery weight at age
plotCount <- plotCount + 1
png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_", "weightFisheryt", ".png")),
	width = plotsizes[1], height = plotsizes[2])
plot(main.diag, var = "weightFishery")
dev.off()

# Survey weight at age
plotCount <- plotCount + 1
png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_", "weightSurvey", ".png")),
	width = plotsizes[1], height = plotsizes[2])
plot(main.diag, var = "weightAge")
dev.off()

## SSB and Rec comparing models
#plotCount <- plotCount + 1
#png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "a_", "SSB", ".png")),
#	width = plotsizes[1], height = plotsizes[2])
#plot(mods.comb, what = "biomass", combine = TRUE, stack = FALSE)
#dev.off()
#
#png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "b_", "Rec", ".png")),
#	width = plotsizes[1], height = plotsizes[2])
#plot(mods.comb, what = "recruitment", combine = TRUE, stack = FALSE)
#dev.off()

# Model fit to age/length comps
plotCount <- plotCount + 1
png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_%02d_", "AgeCompCatchFits", ".png")),
	width = plotsizes[1], height = plotsizes[2])
plot(main.diag, var = "ageFitsCatch")
dev.off()

plotCount <- plotCount + 1
png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_%02d_", "LengthCompCatchFits", ".png")),
	width = plotsizes[1], height = plotsizes[2])
plot(main.diag, var = "lengthFitsCatch")
dev.off()

plotCount <- plotCount + 1
png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_%02d_", "AgeCompSurveyFits", ".png")),
	width = plotsizes[1], height = plotsizes[2])
plot(main.diag, var = "ageFitsSurvey")
dev.off()

# Predicted vs observed indices
plotCount <- plotCount + 1
png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_", "IndexFits", ".png")),
	width = plotsizes[1], height = plotsizes[2])
plot(main.diag, var = "predictedObservedIndices")
dev.off()

# Fishery mean age
plotCount <- plotCount + 1
png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_", "FishMeanAge", ".png")),
	width = plotsizes[1], height = plotsizes[2])
plot(main.diag, var = "fisheryMeanAge")
dev.off()

# Survey mean age
plotCount <- plotCount + 1
png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_", "SurveyMeanAge", ".png")),
	width = plotsizes[1], height = plotsizes[2])
plot(main.diag, var = "surveyMeanAge")
dev.off()

# Survey mean age
plotCount <- plotCount + 1
png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_", "FishMeanLength", ".png")),
	width = plotsizes[1], height = plotsizes[2])
plot(main.diag, var = "fisheryMeanLength")
dev.off()

# Forecast graph; abline v=modendYr
plotCount <- plotCount + 1
png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_", "PredictSSB", ".png")),
	width = plotsizes[1], height = plotsizes[2])
plot(main.diag, var = "ssbPrediction",std=TRUE)
dev.off()

plotCount <- plotCount + 1
png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_", "PredictCatch", ".png")),
	width = plotsizes[1], height = plotsizes[2])
plot(main.diag, var = "catchPrediction")
dev.off()

# Summary of stock status
plotCount <- plotCount + 1
png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_", "Summary", ".png")),
	width = plotsizes[1], height = plotsizes[1])
plot(main.diag, var = "summarySheet")
dev.off()

## Retro
#load(file = paste0("results/", "model_", unlist(strsplit(modnm, split = "d"))[2], "_retrospective", ".RData"))
#
#plotCount <- plotCount + 1
#png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "a_", "Retro_SSB", ".png")),
#	width = plotsizes[1], height = plotsizes[2])
#plot(output, var = "SSB")
#dev.off()
#
#png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "b_", "Retro_Rec", ".png")),
#	width = plotsizes[1], height = plotsizes[2])
#plot(output, var = "R")
#dev.off()
#
## Recruitment likelihood profile
#plotCount <- plotCount + 1
#png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_", "Piner_Rec", ".png")),
#	width = plotsizes[1], height = 700)
#
#layoutmat <- matrix(c(1,2,
#					  3,3),
#					  nrow = 2, ncol =2, byrow = TRUE)
#layout(layoutmat)
#par(mar = c(2,2,1,1), xpd = TRUE, oma = c(3,3,1,1))
#modvec <- c(modnm, mods2compare)
#nlikes <- nrow(summary(mainmod)$like)
#modlike <- matrix(ncol = Nrecs, nrow = nlikes)
#Bend <- matrix(nrow = length(modvec), ncol = Nrecs)
#deltmat <- array(dim = c(length(modvec), nlikes, Nrecs))
#for(Nmod in 1:length(modvec)) {
#	for(i in 1:Nrecs) {
#	  dir.mod <- paste(modvec[Nmod],"piner", i, sep =".")
#	  modres <- readJJM(dir.mod, path="config", input="input")
#	
#	  modlike[,i] <- summary(modres)$like
#	
#	  Biomat <- modres[[1]]$output$Stock_1$SSB
#	  Bend[Nmod, i] <- Biomat[(nrow(Biomat)-1), 2]
#	}
#
#	for(lk in 1:nlikes) {
#  		deltmat[Nmod,lk,] <- modlike[lk,] - min(modlike[lk,])
#	}
#}
#
#xlims <- range(Bend)
#ylims <- range(deltmat)
#
#for(Nmod in 1:length(modvec)) {
#	plot(0, type = "n", xlim = xlims, ylim = ylims*1.1, 
#	  ylab = "Change in Likelihood", xlab = "Final Year SSB")
#	cls <- rainbow((nlikes-1))
#	cls <- c(cls, "black")
#	for(i in 1:nlikes) {
#		lines(x = Bend[Nmod,], y = deltmat[Nmod,i,], col = cls[i], lwd = 2, type = "o")
#	}
#	abline(h = 0, col = "grey", xpd = FALSE)
#}
#
#plot(0, type = "n", xlim = xlims, ylim = ylims*1.3, 
#  ylab = "Change in Likelihood", xlab = "Final Year SSB")
#
#for(Nmod in 1:length(modvec)) {
#	lines(x = Bend[Nmod,], y = deltmat[Nmod,nlikes,], col = cls[i], lwd = 2, type = "o", lty = (1+Nmod))
#}
#abline(h = 0, col = "grey", xpd = FALSE)
#legend("top",legend = rownames(summary(modres)$like), col = cls, lty = 1, pch = 1,
#		lwd = 2, ncol = 3, bty = "n")
#
#legend("bottomright", legend = modvec, lty = (1:length(modvec))+1, col = cls[i], pch = 1, lwd = 2, bty = "n")
#mtext(side = 1, "Final Year SSB", outer = TRUE, line = 1)
#mtext(side = 2, "Change in Likelihood", outer = TRUE, line = 1)
#dev.off()
#
## Natural mortality likelihood "boob" plot
#plotCount <- plotCount + 1
#modres <- NULL
#parvec <- NULL
#Bend <- NULL
#
#for(i in 1:Nreps) {
#  dir.mod <- paste(modnm, par2change, i, sep =".")
#  modres[[i]] <- readJJM(dir.mod, path="config", input="input")
#
#  if(i == 1) {
#    nlikes <- nrow(summary(modres[[i]])$like)
#  	modlike <- matrix(ncol = Nreps, nrow = nlikes)
#  }
#
#  modlike[,i] <- summary(modres[[i]])$like
#
#  Biomat <- modres[[i]][[1]]$output$Stock_1$SSB
#  Bend[i] <- Biomat[(nrow(Biomat)-1), 2]
#
#  parex <- paste0("modres[[i]][[1]]$parameters$", par2change)
#  parvec[i] <- eval(parse(text = parex))
#}
#
#deltmat <- matrix(ncol = ncol(modlike), nrow = nrow(modlike))
#for(i in 1:nlikes) {
#  deltmat[i,] <- modlike[i,] - min(modlike[i,])
#}
#
#png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_", "Mprofile", ".png")),
#	width = plotsizes[1], height = plotsizes[2])
#plot(0, type = "n", xlim = range(parvec), ylim = range(deltmat)*1.1, 
#      ylab = "Change in Likelihood", xlab = par2change)
#cls <- rainbow(nlikes - 1)
#cls <- c(cls, "black")
#
#for(i in 1:nlikes) {
#  lines(x = parvec, y = deltmat[i,], col = cls[i], lwd = 2, type = "o")
#}
#abline(h = 0, col = "grey")
#
#legend("topright",legend = rownames(summary(modres[[1]])$like), col = cls, lty = 1, pch = 1, lwd = 2)
#dev.off()
#
## 2-stock hypothesis SSB and recruitment
plotCount <- plotCount + 1
png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "a_", "2stkSSB", ".png")),
	width = plotsizes[1], height = plotsizes[2])
plot(mod.2stk, what = "biomass", stack = FALSE)
dev.off()

png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "b_", "2stkRec", ".png")),
	width = plotsizes[1], height = plotsizes[2])
plot(mod.2stk, what = "recruitment", stack = FALSE)
dev.off()

# 2-stock hypothesis summary
plotCount <- plotCount + 1
png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_%02d_", "2stkSummary", ".png")),
	width = plotsizes[1], height = plotsizes[2])
plot(diag.2stk, var = "summarySheet")
dev.off()
#
## History of assessment and SSB
#
## Heat map for age/length comps?
#
## Ratio of fish vs unfished biomass?
#
## Stock-recruit plot?
#plotCount <- plotCount + 1
#png(filename = file.path(dir.main, dir.plots, paste0(modnm, "_Fig", plotCount, "_", "StockRecruit", ".png")),
#	width = plotsizes[1], height = plotsizes[2])
#plot(main.diag, var = "stockRecruitment")
#dev.off()

