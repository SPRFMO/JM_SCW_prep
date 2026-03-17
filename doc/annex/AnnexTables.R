# Code to make some of tables for CJM technical annex
# Based on technical annex for SC03 and discussion with M. Pastoors
# Still clunky,  but records functions used to generate values in the tables
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
library(tidyverse)

#----------------------
# User-specified inputs
#----------------------

dir.main <- getwd()
dir.plots <- "annex plots"
if(!dir.exists(dir.plots)) dir.create(dir.plots)

h1nm <- "h1_1.02"

h2nm <- "h2_1.02"

#------------------
# Reading in models
#------------------
h1_mod <- readJJM(h1nm, path = "config", input = "input")

h2_mod <- readJJM(h2nm,path = "config", input = "input")

#-------------------


################
# SSB Table Update
#################
yrs <- which(h1_mod[[1]]$output$Stock_1$SSB[,1] %in% c(1970,max(h1_mod[[1]]$output$Stock_1$SSB[,1])-1))
round(h1_mod[[1]]$output$Stock_1$SSB[yrs[1]:yrs[2],2])


# Estimated begin-year numbers at age
h1_mod[[1]]$output$Stock_1$N
h2_mod[[1]]$output$Stock_1$N
h2_mod[[1]]$output$Stock_2$N

# Estimated total fishing mortality at age
h1_mod[[1]]$output$Stock_1$TotF
h2_mod[[1]]$output$Stock_1$TotF
h2_mod[[1]]$output$Stock_2$TotF

# Summary of results
y_ind <- h1_mod[[1]]$output[[1]]$SSB[,1] %in% h1_mod[[1]]$output[[1]]$msy_mt[,1]
res_h1 <- data.frame(yr  = h1_mod[[1]]$output[[1]]$msy_mt[,1]) %>%
                  mutate(
                    landings  = round(as.vector(rowSums(h1_mod[[1]]$data$Fcaton)), 0),
                    ssb       = round(h1_mod[[1]]$output[[1]]$SSB[y_ind,2], 0),
                    r         = round(h1_mod[[1]]$output[[1]]$R[,2], 0),
                    fmort     = round(h1_mod[[1]]$output[[1]]$msy_mt[,6], 2),
                    fmsy      = round(h1_mod[[1]]$output[[1]]$msy_mt[,5], 2),
                    ssbmsy    = round(h1_mod[[1]]$output[[1]]$msy_mt[,10], 0),
                    )
res_h1

y_ind <- h2_mod[[1]]$output[[1]]$SSB[,1] %in% h2_mod[[1]]$output[[1]]$msy_mt[,1]
res_h2_1 <- data.frame(yr  = h1_mod[[1]]$output[[1]]$msy_mt[,1]) %>%
                  mutate(
                    landings  = round(as.vector(rowSums(h2_mod[[1]]$data$Fcaton)), 0),
                    ssb       = round(h2_mod[[1]]$output[[1]]$SSB[y_ind,2], 0),
                    r         = round(h2_mod[[1]]$output[[1]]$R[,2], 0),
                    fmort     = round(h2_mod[[1]]$output[[1]]$msy_mt[,6], 2),
                    fmsy      = round(h2_mod[[1]]$output[[1]]$msy_mt[,5], 2),
                    ssbmsy    = round(h2_mod[[1]]$output[[1]]$msy_mt[,10], 0),
                    )
res_h2_1

y_ind <- h2_mod[[1]]$output[[2]]$SSB[,1] %in% h2_mod[[1]]$output[[2]]$msy_mt[,1]
res_h2_2 <- data.frame(yr  = h1_mod[[1]]$output[[1]]$msy_mt[,1]) %>%
                  mutate(
                    landings      = round(as.vector(rowSums(h2_mod[[1]]$data$Fcaton)), 0),
                    ssb       = round(h2_mod[[1]]$output[[2]]$SSB[y_ind,2], 0),
                    r         = round(h2_mod[[1]]$output[[2]]$R[,2], 0),
                    fmort     = round(h2_mod[[1]]$output[[2]]$msy_mt[,6], 2),
                    fmsy      = round(h2_mod[[1]]$output[[2]]$msy_mt[,5], 2),
                    ssbmsy    = round(h2_mod[[1]]$output[[2]]$msy_mt[,10], 0),
                    )
res_h2_2


#########
# To get fished over unfished total biomass
#########
h1_uf_b <- rev(h1_mod[[1]]$output[[1]]$TotBiom_NoFish[,2])[1]
h1_f_b <- rev(h1_mod[[1]]$output[[1]]$TotBiom[,2])[1]

h1_f_b / h1_uf_b

h2_1_uf_b <- rev(h2_mod[[1]]$output[[1]]$TotBiom_NoFish[,2])[1]
h2_1_f_b <-  rev(h2_mod[[1]]$output[[1]]$TotBiom[,2])[1]

h2_1_f_b / h2_1_uf_b

h2_2_uf_b <- rev(h2_mod[[1]]$output[[2]]$TotBiom_NoFish[,2])[1]
h2_2_f_b <-  rev(h2_mod[[1]]$output[[2]]$TotBiom[,2])[1]

h2_2_f_b / h2_2_uf_b

#########
# Current SSB (year, SSB, SSB.sd, lower bound, upper bound)
#########
h1_mod[[1]]$output[[1]]$SSB
(rev(h1_mod[[1]]$output[[1]]$SSB[,2])[2] / 1000) %>% round(digits=2)
h2_mod[[1]]$output[[2]]$SSB


######
# Estimated b/bmsy
# Yr Fspr 1-Fspr F/Fmsy Fmsy F Fsprmsy MSY MSYL Bmsy Bzero SSB B/Bmsy
######
h1_mod[[1]]$output[[1]]$msy_mt

fixed_bmsy(h1_mod)[[1]]$output[[1]]$msy_mt[1,10]

fixed_bmsy(h2_mod)[[1]]$output[[1]]$msy_mt[1,10]
fixed_bmsy(h2_mod)[[1]]$output[[2]]$msy_mt[1,10]

# Blim
min(h1_mod[[1]]$output[[1]]$SSB_NoFishR[,2]) * h1_mod[[1]]$output[[1]]$SSB_fut_1[1,2] # SSB given in kt
