
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

library(tidyverse)
library(flextable)
install.packages("libr")
library(libr)
install.packages("fmtr")
library(fmtr)
library(abind)
library(openMSE)

source("../mse/Source_JM2OM.r")   # load JM2OM source code


# --- Conditioning Model inputs / outputs --------------------------------------

mod <- jjmR::readJJM("h1_1.00", path = "config", input = "input") # data, pars, MLE output
mceval_file = "~/Downloads/mceval.csv"                        # location of MCMC output of F and N at age
getwd()

df1<-readr::read_csv(mceval_file)
head(df1)
glimpse(df2)
df2 <- sample_frac(df1, size = .1)
df2 <- df1 |> filter(mcdraw<33)
write_csv(df2,"mceval.csv")


# --- Make an OM using JM2OM ---------------------------------------------------

OM = JM2OM(mod, mceval_file,
           proyears = 50,          # 50 projection years
           interval = 2,           # update management every 2 years
           nsim = 32,              # only 32 sims for demo purposes
           datastart = 2012,       # only the last 10 years of data are submitted to derive statistical properties for index simulation
           check = T)              # visual check that reconstructed openMSE N and F matches conditioning model estimates
                                   #   NOTE: there is an apparent 1-year lag in the numbers at age in the
                                   #   'check' figure, as openMSE numbers are reported as start of year.


# --- plot the OM --------------------------------------------------------------

plot(OM)                           # This is mostly just for seeing time series but documents all aspects of the model
                                   #   NOTE: observation error characteristics for indices used in the assessment are
                                   #   calculated from the simulated observed data (see below)


# --- Make an historical reconstruction to get data format ---------------------

Hist = runMSE(OM, Hist=T)          # you can use this from now on to avoid spool-up calculations
Data = Hist@Data                   # the format of data that will enter your management procedures (useful for designing MPs)
Ind = Data@AddInd                  # simulated dataset 1 (all simulations are identical for historical period, but projected data vary)
dim(Ind)                           # simulation, Index, year
OM@Misc$Inames                     # I stored the index names here in case they are useful
Hist@SampPars$Obs$AddInd_Stat      # autocorrelation and CV for the 7 indices based on historical fit (only #3 & #7 seem ok!)


# --- Run simple projections ---------------------------------------------------

avail('MP')                        # the various MP types of openMSE

                                   # no fish,    current effort, current catch, FMSY fractions
proj_simple = Project(Hist, MPs = c("NFref",  "curE",         "CurC",        "FMSYref","FMSYref75","FMSYref50"))
Pplot(proj_simple)                 # generic projection plot of SSB/SSBMSY and F/FMSY
NOAA_plot2(proj_simple)            # NOAA plot from Carribean Council
DFO_proj(proj_simple)              # DFO projection 'Kobe' plot


# --- Design a simple index ratio MP -------------------------------------------

PPD_75 = proj_simple@PPD[[5]]                  # 5th MP was FMSYref 75
endy = 91:100                                  # last 10 years of projection
muIend = apply(PPD_75@AddInd[,,endy],1:2,mean) # average value of each simulated index over last 10 years
Catend = apply(PPD_75@Cat[,endy],1,mean)
rate = Catend / muIend
deltas = round(apply(rate,2,mean,na.rm=T),3)
TRP = apply(muIend,2,mean)


myIRMP = function(x, Data, reps=1, smthyrs = 3,                                # x is the simulation number
                  deltas =  c(NA,  1.824, 2745.6, NA,  3.736,  100.10, 0.545), # multiplier C = delta * I
                  TRP =     c(NA,  781,   0.249,  NA,  177,    7.99,   1194),  # 'target' index level
                  mult = 0.8,                                                  # multiplier of initial delta
                  useInd = c(NA, NA, 1, NA, NA, NA, 1)){                       # only #3 and #7 seems to have low CV....

  thisyr = length(Data@Year)                                                   # index of most recent year in data
  subI = Data@AddInd[x,,thisyr-((smthyrs-1):0),drop=F]                         # take average of last smthyrs years
  curI = apply(subI, 2, mean)                                                  # simple average (could be a smoother etc)
  mod = curI / TRP; mod[mod>1]=1                                               # linear decline in catch below 'TRP' index level
  Rec=new('Rec')                                                               # make an object of class Recommendation
  Rec@TAC = mean(curI * deltas * useInd * mod, na.rm=T) * mult                 # populate the TAC slot with the mean catch rec across indices
  Rec
}

class(myIRMP) = "MP"

proj_IR = Project(Hist, c("myIRMP","FMSYref75"),checkMPs=F)
Pplot(proj_IR)


# --- Assessments as MPs -------------------------------------------------------

avail('MP')                             # some premade data rich assessment MPs in here
avail('Assess')                         # possible assessment methods
avail('HCR')                            # possible HCRs
SSSP_4010 = make_MP("SP_SS","HCR40_10") # state-space surplus production with 40:10 rule

# this will take 10 mins (could try with parallel computation using setup())
proj_Assess = Project(Hist, c("SSSP_4010","SCA_MSY","FMSYref75"),checkMPs=F) #SCA_MSY is a statistical catch at age model
Pplot(proj_Assess)


# --- Other demos (size etc) ---------------------------------------------------




# --- Performance metrics ------------------------------------------------------



# --- Exceptional circumstances ------------------------------------------------

# check out the github repo at blue-matter/ECP

PPD = proj_IR@PPD[[1]]


# --- NOTES --------------------------------------------------------------------

setup() # for parallel computation run this line before runMSE or Project (sometimes faster!)
lw_a = 0.007778994e-3 ; # LW parameters from Talcahuano sampling, 2008
lw_b = 3.089248476 ; # LW parameters from Talcahuano sampling, 2008

# =============== END =============================================================================
