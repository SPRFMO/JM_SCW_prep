
# ===========================================================================================================
# === Script to convert Jim's Jack Mackerel Assessment to an OpenMSE operating model ========================
# ===========================================================================================================

# Tom Carruthers
# Feb 2024

# --- installation -------------------------------------------------------------

# devtools::install_github("SPRFMO/jjmr")
# devtools::install_github("SPRFMO/FLjjm")

# --- Prerequisites ------------------------------------------------------------

setwd("~/_mymods/SPRFMO/jjm/assessment")
# setwd("C:/GitHub/jjm/assessment") # Tom WD

library(tidyverse)
library(flextable)
# install.packages("libr")
library(libr)
# install.packages("fmtr")
library(fmtr)
library(abind)
library(data.table)
library(openMSE)

source("../mse/Source_JM2OM.r")   # load JM2OM source code


# --- Conditioning Model inputs / outputs --------------------------------------

run_loc = "../mse/mcmc/om1/mcmc" #Change this locale 
# run_loc = "C:/temp/JM_mcmc/OM1" # Tom locale

mod <- jjmR::readJJM("h1_ls", output = run_loc, path = run_loc, input = run_loc)
mceval_file = paste0(run_loc, "/mceval.csv" ) # location of MCMC output of F (by fleet) and N at age


# vvvvv  Jim Code vvvvvvvvvvvvvvvvvvvvvvvvv

df1<-readr::read_csv(mceval_file)
head(df1)
df2 <- sample_frac(df1, size = .1)
df2 <- df1 |> filter(mcdraw<33)
glimpse(df2)
write_csv(df2,"mceval.csv")

# ^^^^^ Jim Code ^^^^^^^^^^^^^^^^^^^^^^^^^^


# --- Make an OM using JM2OM ---------------------------------------------------

OM = JM2MOM(mod, mceval_file,
            proyears = 50,          # 50 projection years
            interval = 2,           # update management every 2 years
            nsim = 32,              # only 32 sims for demo purposes
            datastart = 2007,       # only the last 15 years of data are submitted to derive statistical properties for index simulation
            check = T)              # visual check that reconstructed openMSE N and F matches conditioning model estimates
                                   

# --- plot the OM --------------------------------------------------------------

# plot(OM)                         # This is mostly just for seeing time series but documents all aspects of the model
                                   #   NOTE: observation error characteristics for indices used in the assessment are
                                   #   calculated from the simulated observed data (see below)


# --- Make an historical reconstruction to get data format ---------------------

Hist = multiMSE(OM, Hist=T)                 # you can use this from now on to avoid spool-up calculations
stockno = 1; fleetno = 1                    # specify which stock and fleet in the multiOM
Data = Hist[[stockno]][[fleetno]]@Data      # the format of data that will enter your management procedures (useful for designing MPs)
Ind = Data@AddInd                           # simulated dataset 1 (all simulations are identical for historical period, but projected data vary)
dim(Ind)                                    # simulation, Index, year
Inam = OM@Fleets[[stockno]][[fleetno]]@Misc$Inames # I stored the index names here in case they are useful
Istat = Hist[[stockno]][[fleetno]]@SampPars$Obs$AddInd_Stat     # autocorrelation and CV for the 7 indices based on historical fit (only #3 & #7 seem ok!)
names(Istat) = Inam
lapply(Istat,function(x)if(length(x)>0)apply(x,2,mean))


# --- Run simple projections ---------------------------------------------------

avail('MP')                          # the various MP types of openMSE
                                     # FMSY fractions
proj_simple = ProjectMOM(Hist, MPs = c("FMSYref","FMSYref75"))
plot_proj_JJM(proj_simple)            # generic projection plots of SSB/SSBMSY, F, Yield, Yield by fleet


# --- Design a simple index ratio MP -------------------------------------------

# NOTE:
# make sure to read the documentation on multiMSE for more about multi-MPs 
# https://openmse.com/features-multimse/multi-mp/
# In this instance we will just make a single recommendation using the aggregate
#   data (all fleets combined) and let multiMSE divide it up by recent catch
#   fractions by fleet

PPD_75 = sData = proj_simple@PPD[[stockno]][[fleetno]][[2]]      # 2nd MP was FMSYref 75
endy = 91:100                                                    # last 10 years of projection
muIend = apply(PPD_75@AddInd[,,endy],1:2,mean)                   # average value of each simulated index over last 10 years
Catend = apply(PPD_75@Cat[,endy],1,mean)                         # average catch observed over last 10 years of projection    
rate = Catend / muIend                                           # catch per index
deltas = round(apply(rate,2,mean,na.rm=T),3)                     # mean catch per index
TRP = apply(muIend/2,2,mean)                                       # lets say we want a control rule where we aim for mean FMSYref75 levels

MLE = mod[[1]]$output$Stock_1                                                  # get model for calculating allocations
cat23 = sapply(MLE[grepl("Obs_catch",names(MLE))],function(x)x[length(x)])     # observed catch in final year by fleet
allo = round(cat23/sum(cat23),4)                                               # implied allocation

smooth<-function(xx,plot=F,enp.mult=0.25,plotname=""){
  tofill<-!is.na(xx)
  xx[xx==0]<-1E3
  predout<-rep(NA,length(xx))
  dat<-data.frame(x=1:length(xx),y=log(xx))
  enp.target<-sum(tofill)*enp.mult
  out<-loess(y~x,dat=dat,enp.target=enp.target)
  predout[tofill]<-exp(predict(out))
  if(plot){
    plot(xx,type="p",xlab="x",ylab="y",main=plotname)
    lines(predout,col="#ff000090",lwd=2)
  }
  predout
}

smooth(PPD_75@AddInd[1,6,1:54],plot=T) # check smoothing for Peru CPUE (index 6) up to 2023
smooth(PPD_75@AddInd[1,2,1:54],plot=T) # check smoothing for Chilean CPUE (index 2) up to 2023



# Specify MP using quantities / functions above
IRMP = function(x, DataList, reps=1, enp.mult = 0.25,                          # x is the simulation number
                  deltas =  c(NA,  0.529, 2012.446, NA, 2.801, 56.531, 0.419), # multiplier C = delta * I
                  TRP =     c(NA,  182.5, 0.055, NA, 36.77, 1.78, 274.565),    # 'target' index level
                  mult = 1.5,                                                  # tuning parameter - multiplier of initial delta
                  useInd = c(NA, NA, NA, NA, NA, 1, NA),                       # runs off Peru_CPUE Index only (CV ~34%)
                  allo = c(0.0709,  0.6489,  0.2028, 0.0774)){                 # fleet TAC allocation is same as 2023    

  Data = DataList[[1]][[1]]                                                    # stock 1, fleet 1 has aggregate data
  thisyr = length(Data@Year)                                                   # index of most recent year in data
  subI = Data@AddInd[x,,]                                                      # take average of last smthyrs years
  smoothI = apply(subI, 1, smooth,enp.mult=enp.mult)                           # smoothed index
  curI = smoothI[nrow(smoothI), ]                                              # get most recent smoothed index
  mod = curI / TRP; mod[mod>1]=1                                               # linear decline in catch below 'TRP' index level
  TACtot = mean(curI * deltas * useInd * mod, na.rm=T) * mult                  # f
  
  out = list()                                                                 # nested list of management recommendations [[stock]][[fleet]]
  out[[1]] = list()                                                            # only one stock
  for(ff in 1:length(allo)){
    Rec=new('Rec')                                                             # make an object of class Recommendation
    Rec@TAC =  TACtot * allo[ff]                                               # allocated catch
    out[[1]][[ff]] = Rec
  }
  
  out
}

class(IRMP) = "MMP"

IRMP2 = IRMP3 = IRMP                                # copy IRMP
formals(IRMP2)$mult = 2                             # more aggressive tuning parameter
formals(IRMP3)$useInd = c(NA, 1, NA, NA, NA, 1, NA) # add Chilean CPUE
class(IRMP2) = class(IRMP3) = "MMP"                 # this tells multiMSE to send a datalist[[stock]][[fleet]] to the MMP


# --- Run the simple index ratio MPs -------------------------------------------

proj_IR = ProjectMOM(Hist, c("IRMP","IRMP2","IRMP3"), checkMPs=F)
plot_proj_JJM(proj_IR)                     # plots SB/SBMSY, Apical F, Yield (rel today), Yield by fleet


# --- Other demos (size etc) ---------------------------------------------------

  # coming soon!

# --- Performance metrics ------------------------------------------------------

  # coming soon!

# --- Exceptional circumstances ------------------------------------------------

# check out the github repo at blue-matter/ECP

stockno = 1; fleetno =1; MPno=1
PPD = proj_IR@PPD[[stockno]][[fleetno]][[MPno]]


# --- NOTES --------------------------------------------------------------------

setup() # for parallel computation run this line before runMSE or Project (sometimes faster!)
lw_a = 0.007778994e-3 ; # LW parameters from Talcahuano sampling, 2008
lw_b = 3.089248476 ; # LW parameters from Talcahuano sampling, 2008

# =============== END =============================================================================
