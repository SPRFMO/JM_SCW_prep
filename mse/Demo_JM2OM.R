
# ===========================================================================================================
# === Script to convert Jim's Jack Mackerel Assessment to an openMSE operating model ========================
# ===========================================================================================================

# --- installation -------------------------------------------------------------

# devtools::install_github("SPRFMO/jjmr")
# devtools::install_github("SPRFMO/FLjjm")

# --- Prerequisites ------------------------------------------------------------

setwd("C:/GitHub/jjm/assessment")

library(tidyverse)
library(flextable)
library(libr)
library(fmtr)
library(abind)
library(openMSE)

source("..//mse/Source_JM2OM.r") # load JM2OM source code


# --- Conditioning Model inputs / outputs --------------------------------------

mod <- jjmR::readJJM("h1_1.00", path = "config", input = "input") # Data, pars, MLE output
mceval_file = "C:/temp/JM_mcmc/mceval.csv" # Get MCMC output of F and N at age


# --- Make an OM using JM2OM ---------------------------------------------------

OM = JM2OM(mod, mceval_file, 
           proyears = 50,          # 50 projection years
           interval = 2,           # update management every 2 years
           nsim = 32,              # Only 32 sims for demo purposes
           check = T)              # visual check that reconstructed openMSE N and F matches conditioning model estimates
                                   #   note: there is an apparent 1-year lag as openMSE numbers are reported as start of year


# --- plot the OM --------------------------------------------------------------

plot(OM)                           # This is mostly just for seeing time series but documents all aspects of the model
                                   #   note: observation error characteristics for indices used in the assessment are 
                                   #   calculated from the simululated observed data (see below)


# --- Make an historical reconstruction to get data format ---------------------

Hist = runMSE(OM, Hist=T)     # you can use this from now on to avoid spool-up calculations
Data = Hist@Data              # the format of data that will enter your management procedures
Ind = Data@AddInd             # simulated dataset 1 (all simulations are identical for historical period, but projected data vary)
dim(Ind)                      # simulation, Index, year
OM@Misc$Inames                # I stored the index names here in case they are useful
Hist@SampPars$Obs$AddInd_Stat # Autocorrelation and CV for the 7 indices based on historical fit (only #7 seems ok!)


# --- Run simple projections ---------------------------------------------------

avail('MP')                   # the various MP types of openMSE

#                                 no fish,    current effort, current catch, FMSY fractions
proj_simple = Project(Hist, MPs = c("NFref",  "curE",         "CurC",        "FMSYref","FMSYref75","FMSYref50"))
Pplot(proj_simple)
NOAA_plot2(proj_simple)
DFO_proj(proj_simple)


# --- Design a simple index ratio MP -------------------------------------------

PPD_75 = proj_simple@PPD[[5]] # 5th MP was FMSYref 75
endy = 91:100  # last 10 years of projection 
muIend = apply(PPD_75@AddInd[,,endy],1:2,mean) # average value of each simulated index over last 10 years
Catend = apply(PPD_75@Cat[,endy],1,mean)
rate = Catend / muIend
deltas = apply(rate,2,mean)


myIRMP = function(x, Data, reps=1, smthyrs = 3, # x is the simulation number
                  deltas =  c(0.218,  2.264, 2445.3, 0.229,  23.060,   137.22, 0.467),
                  mult = 3,
                  useInd = c(F, F, F, F, F, F, T)){ 
  
  thisyr = length(Data@Year)
  curI = apply(Data@AddInd[x,,thisyr-((smthyrs-1):0),drop=F],2,mean) # take average of last smthyrs years

  Rec=new('Rec') # make an object of class Recommendation
  Rec@TAC = mean(curI * deltas * useInd, na.rm=T) * mult # populate the TAC slot with the mean catch rec across indices
  Rec 
}
class(myIRMP) = "MP"

proj_IR = Project(Hist, c("myIRMP","FMSYref75"))
Pplot(proj_IR)



#

