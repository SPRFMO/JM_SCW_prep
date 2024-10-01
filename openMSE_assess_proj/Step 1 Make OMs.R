# ===========================================================================================================
# === Script to make OMs for MCMC assessment projections using jjm as an OpenMSE operating model ============
# ===========================================================================================================

# Tom Carruthers
# October 2024

# --- prerequisites ------------------------------------------------------------

setwd("C:/GitHub/jjm") # Tom WD

library(tidyverse)
library(flextable)
library(libr)
library(fmtr)
library(abind)
library(data.table)
library(openMSE)

source("mse/Source_JM2OM.r")   # load JM2OM source code



# --- Conditioning Model inputs / outputs --------------------------------------

GetOM_Jim <- function(run_loc="C:/temp/JM_mcmc/mse/mcmc/om1/mcmc", hyp="h1_ls", nsim=200, interval = 5) {
  mod <- jjmR::readJJM(hyp, output = run_loc, path = run_loc, input = run_loc)
  mceval_file = paste0(run_loc, "/mceval.csv" ) # location of MCMC output of F (by fleet) and N at age
  return (JM2MOM(mod, mceval_file,
                 proyears = 15,          # 50 projection years
                 interval = interval,    # update management every 2 years
                 nsim = nsim,            # only 32 sims for demo purposes
                 datastart = 2007,       # only the last 15 years of data are submitted to derive statistical properties for index simulation
                 check = F)              # visual check that reconstructed openMSE N and F matches conditioning model estimates
  )
}

OM_1 <- GetOM_Jim("C:/temp/JM_mcmc/mse/mcmc/om1/mcmc", "h1_ls", nsim = 196, interval = 5) # an example rip of mcmc outputs to an OM

# Check fleet allocation
OM_1@Allocation[[1]]   # stock 1, [simulation, fleet] based on last historical year of catches

saveRDS(OM_1,"C:/GitHub/jjm/openMSE_assess_proj/OM_1.rda")


# === End of script ===========================================================================



