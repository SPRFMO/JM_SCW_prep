# ===========================================================================================================
# === Script to summarize MCMC assessment projections in an decision table ==================================
# ===========================================================================================================

# Tom Carruthers
# October 2024

# --- Prerequisites ------------------------------------------------------------

library(openMSE)
setwd("C:/GitHub/jjm/openMSE_assess_proj") # Tom WD
source('MP_spawn.R')
yrs = 2023+1:15

# --- load Projections ---------------------------------------------------------

Proj = readRDS("Proj.rda")


# --- Extract projection data --------------------------------------------------

# Dimensions are sim, stock, fleet, MP, projection year
Catch = apply(Proj@Catch[,1,,,],c(1,3,4),sum) # sum over fleets for stock 1 (only 1 stock)

# Dimensions are sim, stock, MP, projection year
Bio = Proj@B[,1,,]
SSBrel = Proj@SB_SBMSY[,1,,]


# --- Calculate tabulated values -----------------------------------------------

# Catches
mu_Cat_24 = apply(Catch[,,1],2,mean) # mean by MP
mu_Cat_25 = apply(Catch[,,2],2,mean) 

# Biomass

mu_B_25 = apply(Bio[,,2],2,mean)     
mu_B_29 = apply(Bio[,,6],2,mean)
mu_B_33 = apply(Bio[,,10],2,mean)

# Biomass risk

PBMSY_25 = apply(SSBrel[,,2]>1,2,mean)*100
PBMSY_29 = apply(SSBrel[,,6]>1,2,mean)*100
PBMSY_33 = apply(SSBrel[,,10]>1,2,mean)*100


# --- Tabulate -----------------------------------------------------------------

DF = data.frame(mu_Cat_24, mu_Cat_25, mu_B_25, PBMSY_25, mu_B_29, PBMSY_29, 
                mu_B_33, PBMSY_33, row.names = Proj@MPs[[1]] )
DT = data.table()


# === End of script ===========================================================================
