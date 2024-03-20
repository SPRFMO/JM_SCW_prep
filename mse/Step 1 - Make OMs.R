# ===========================================================================================================
# === Script to make four OMs using Jim's Jack Mackerel Assessment to an OpenMSE operating model ============
# ===========================================================================================================

# Tom Carruthers
# March 2024

# --- prerequisites ------------------------------------------------------------

setwd("~/_mymods/SPRFMO/jjm/assessment")
# setwd("C:/GitHub/jjm/assessment") # Tom WD

library(tidyverse)
library(flextable)
library(libr)
library(fmtr)
library(abind)
library(data.table)
library(openMSE)

source("../mse/Source_JM2OM.r")   # load JM2OM source code

# setwd("C:/temp/JM_mcmc/mse") # Toms WD

# --- Conditioning Model inputs / outputs --------------------------------------

GetOM_Jim <- function(run_loc="../mse/mcmc/om1/mcmc", hyp="h1_ls") {
  mod <- jjmR::readJJM(hyp, output = run_loc, path = run_loc, input = run_loc)
  mceval_file = paste0(run_loc, "/mceval.csv" ) # location of MCMC output of F (by fleet) and N at age
  return (JM2MOM(mod, mceval_file,
                 proyears = 50,          # 50 projection years
                 interval = 2,           # update management every 2 years
                 nsim = 128,             # only 32 sims for demo purposes
                 datastart = 2007,       # only the last 15 years of data are submitted to derive statistical properties for index simulation
                 check = T)              # visual check that reconstructed openMSE N and F matches conditioning model estimates
  )
}

OM_h1_ls <- OM1 <- GetOM_Jim("../mse/mcmc/om1/mcmc", "h1_ls")
OM_h1_ll <- OM2 <- GetOM_Jim("../mse/mcmc/om2/mcmc", "h1_ll")
OM_h1_hl <- OM3 <- GetOM_Jim("../mse/mcmc/om3/mcmc", "h1_hl")
OM_h1_hs <- OM4 <- GetOM_Jim("../mse/mcmc/om4/mcmc", "h1_hs")

# setwd("C:/GitHub/jjm/assessment") # Tom WD

saveRDS(OM_h1_ls,"../mse/OMs/OM_h1_ls.rda")
saveRDS(OM_h1_ll,"../mse/OMs/OM_h1_ll.rda")
saveRDS(OM_h1_hl,"../mse/OMs/OM_h1_hl.rda")
saveRDS(OM_h1_hs,"../mse/OMs/OM_h1_hs.rda")


# === End of script ===========================================================================



