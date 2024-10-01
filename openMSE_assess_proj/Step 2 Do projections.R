# ===========================================================================================================
# === Script to undertake MCMC assessment projections using jjm as an OpenMSE operating model ===============
# ===========================================================================================================

# Tom Carruthers
# October 2024


# --- Prerequisites ------------------------------------------------------------

library(openMSE)
setwd("C:/GitHub/jjm/openMSE_assess_proj") # Tom WD
source('MP_spawn.R')




# --- define projection scenarios ----------------------------------------------

# TAC MPs
TAC = function(x, Data, reps=1, TAC2024 = 1242, TACmult = 1){   Rec = new('Rec');  Rec@TAC = TAC2024 * TACmult;  return(Rec)}
class(TAC) = "MP"

TAC_nams = paste0("TAC_",c(100,115,120))
spawnMP("TAC", spawnlist=list(TACmult = c(1, 1.15, 1.20)), spawnnames = TAC_nams)

# F MPs
FM = function(x, Data, reps=1, CurFmult = 1){ Rec = new('Rec'); Rec@Effort = CurFmult; return(Rec)}
class(FM) = "MP"

FM_nams = paste0("FM_",c(75, 100, 125))
spawnMP("FM", spawnlist=list(CurFmult = c(0.75,1,1.25)), spawnnames = FM_nams)

# named list of MPs to test
MPs = c("FMSYref", FM_nams, TAC_nams)


# --- Do Projections -----------------------------------------------------------

# --- OM 1----------------------------------------------------

OM_1 = readRDS("OM_1.rda")
Hist_1 = multiMSE(OM_1, Hist = T) # do the historical spool up including Ref pt calcs
Proj_1 = ProjectMOM(Hist_1, MPs, parallel=T)
saveRDS(Proj_1,"Proj_1.rda")


# --- OM 2----------------------------------------------------

# another OM



# --- OM 3----------------------------------------------------

# another OM





# === End of script ===========================================================================
