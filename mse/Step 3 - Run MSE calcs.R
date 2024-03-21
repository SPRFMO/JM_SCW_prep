# ===========================================================================================================
# === Script to run the specified MPs against all OMs =======================================================
# ===========================================================================================================

# Tom Carruthers
# March 2024

# This is designed as a source script

# --- Prerequisites ------------------------------------------------------------

setwd("~/_mymods/SPRFMO/jjm/assessment")
# setwd("C:/GitHub/jjm/assessment") # Tom WD

library(openMSE)
source("../mse/Step 2 - Specify MPs.r")   # load MPs made in step 2
source("../mse/Source_JM2OM.r")           # load JM2OM source code
getOMs("../mse/OMs/")                     # load OMs  (OMnam, nOM, runnam)  
largedir = "C:/temp/JM_mcmc/"             # somewhere to store big things off the github 
                                          #  (needs /Hists and /MSEs subdirectories)


# --- Run MSE spool up ---------------------------------------------------------

# This just means that you don't have to do all the MSY calcs etc each time
# you want to project some new MPs

for(om in 1:nOM){
  Hist = multiMSE(get(OMnam[om]), Hist = T)
  saveRDS(Hist,paste0(largedir,"Hists/Hist_",runnam[om],".rda"))
} 


# --- Run MSE projections ------------------------------------------------------

for(om in 1:nOM){
  Hist = readRDS(paste0(largedir,"Hists/Hist_",runnam[om],".rda"))
  MSE = ProjectMOM(Hist, IRMPs)
  saveRDS(MSE,paste0(largedir,"MSEs/MSE_",runnam[om],".rda"))
} 




# === End of script ===========================================================================



