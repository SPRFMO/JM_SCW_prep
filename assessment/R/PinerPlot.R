# Making Piner ("Pliny") Plots with JJMS models
# Assumes appropriate input (.ctl | .dat) files are already in place
# Runs the models in parallel and makes plots

#-----------------
# Loading packages
#-----------------

if(!require(devtools)) {install.packages("devtools")}
if(!require(jjmR)) {
	library(devtools)
	devtools::install_github("sprfmo/jjmr")
}
if(!require(foreach)) {install.packages("foreach")}
if(!require(doParallel)) {install.packages("doParallel")}

library(jjmR)
library(foreach)
library(doParallel)

#---------------------
# User-specific inputs
#---------------------

setwd("C:/Users/leeqi/Documents/Github/jjm")
dir.base <- getwd()

registerDoParallel(3) # Number of logical processors you want to use for parallel processes

modname <- "mod1.12"

file.exe <- "jjms.exe"
file.exe <- "jjms"

Nreps <- 9
par2change <- "N_Mort"

file.ctl <- paste(modname, ".ctl", sep = "")
file.dat <- paste(modname, ".dat", sep = "")
dir.ast <- "assessment"
dir.src <- "src"

#---------------------------
# Running models in parallel
#---------------------------

foreach(i = 1:Nreps) %dopar% {
	library(jjmR)
	setwd(file.path(dir.base, dir.ast))

	new.mod <- paste(modname, i, sep =".")
	runJJM(new.mod, path="config", input="input", exec="../src/jjms")
}

#---------------------------------------------------------
# Getting likelihoods and final year SSB from report files
#---------------------------------------------------------
setwd(file.path(dir.base, dir.ast))

modres <- NULL
parvec <- NULL
Bend <- NULL
setwd(file.path(dir.base,dir.ast))

for(i in 1:Nreps) {
  dir.mod <- paste(modname, i, sep =".")
  modres[[i]] <- readJJM(dir.mod, path="config", input="input")

  if(i == 1) {
  	modlike <- matrix(ncol = Nreps, nrow = nrow(summary(modres[[i]])$like))
  }

  modlike[,i] <- summary(modres[[i]])$like

  Biomat <- modres[[i]][[1]]$output$Stock_1$TotBiom
  Bend[i] <- Biomat[nrow(Biomat), 2]

  parex <- paste("modres[[i]][[1]]$parameters$", par2change, sep = "")
  parvec[i] <- eval(parse(text = parex))
}

#----------------------------------
# Making change-in-likelihood table
#----------------------------------
deltmat <- matrix(ncol = ncol(modlike), nrow = nrow(modlike))
for(i in 1:nrow(modlike)) {
  deltmat[i,] <- modlike[i,] - min(modlike[i,])
}

#---------------------------------
# Plotting the likelihood profiles
#---------------------------------
plot(0, type = "n", xlim = range(par2change), ylim = range(deltmat)*1.1, 
      ylab = "Change in Likelihood", xlab = "Natural Mortality")
cls <- rainbow(13)
cls <- c(cls, "black")

for(i in 1:nrow(modlike)) {
  lines(x = parvec, y = deltmat[i,], col = cls[i], lwd = 2, type = "o")
}
abline(h = 0, col = "grey")

legend("topright",legend = rownames(summary(modres[[1]])$like), col = cls, lty = 1, pch = 1, lwd = 2)

#--------------------------
# Plotting the SSB profiles
#--------------------------
dev.new()
plot(x = parvec, y = Bend, col = "black", lwd = 2, type = "o", ylab = "Final Year SSB", xlab = "Natural Mortality")
