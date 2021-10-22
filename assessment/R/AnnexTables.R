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
#
#----------------------
# User-specified inputs
#----------------------

dir.main <- getwd()
dir.plots <- "annex plots"
if(!dir.exists(dir.plots)) dir.create(dir.plots)

h1nm <- "h1_1.05"

h2nm <- "h2_1.05"

#------------------
# Reading in models
#------------------
h1.mod <- readJJM(h1nm, path = "config", input = "input")

h2.mod <- readJJM(h2nm,path = "config", input = "input")

#-------------------


################
# SSB Table Update
#################
yrs <- which(h1.mod[[1]]$output$Stock_1$SSB[,1] %in% c(1970,max(h1.mod[[1]]$output$Stock_1$SSB[,1])-1))
round(h1.mod[[1]]$output$Stock_1$SSB[yrs[1]:yrs[2],2])


# Estimated begin-year numbers at age
h1.mod[[1]]$output$Stock_1$N
h2.mod[[1]]$output$Stock_1$N
h2.mod[[1]]$output$Stock_2$N

# Estimated total fishing mortality at age
h1.mod[[1]]$output$Stock_1$TotF
h2.mod[[1]]$output$Stock_1$TotF
h2.mod[[1]]$output$Stock_2$TotF

# Summary of results
y_ind <- h1.mod[[1]]$output[[1]]$SSB[,1] %in% h1.mod[[1]]$output[[1]]$msy_mt[,1]
res.h1 <- data.frame(yr  = h1.mod[[1]]$output[[1]]$msy_mt[,1]) %>% 
                  mutate(
                    landings  = round(as.vector(rowSums(h1.mod[[1]]$data$Fcaton)), 0),
                    ssb       = round(h1.mod[[1]]$output[[1]]$SSB[y_ind,2], 0),
                    r         = round(h1.mod[[1]]$output[[1]]$R[,2], 0),
                    fmort     = round(h1.mod[[1]]$output[[1]]$msy_mt[,6], 2),
                    fmsy      = round(h1.mod[[1]]$output[[1]]$msy_mt[,5], 2),
                    ssbmsy    = round(h1.mod[[1]]$output[[1]]$msy_mt[,10], 0),
                    )
res.h1

y_ind <- h2.mod[[1]]$output[[1]]$SSB[,1] %in% h2.mod[[1]]$output[[1]]$msy_mt[,1]
res.h2.1 <- data.frame(yr  = h1.mod[[1]]$output[[1]]$msy_mt[,1]) %>% 
                  mutate(
                    landings  = round(as.vector(rowSums(h2.mod[[1]]$data$Fcaton)), 0),
                    ssb       = round(h2.mod[[1]]$output[[1]]$SSB[y_ind,2], 0),
                    r         = round(h2.mod[[1]]$output[[1]]$R[,2], 0),
                    fmort     = round(h2.mod[[1]]$output[[1]]$msy_mt[,6], 2),
                    fmsy      = round(h2.mod[[1]]$output[[1]]$msy_mt[,5], 2),
                    ssbmsy    = round(h2.mod[[1]]$output[[1]]$msy_mt[,10], 0),
                    )
res.h2.1

y_ind <- h2.mod[[1]]$output[[2]]$SSB[,1] %in% h2.mod[[1]]$output[[2]]$msy_mt[,1]
res.h2.2 <- data.frame(yr  = h1.mod[[1]]$output[[1]]$msy_mt[,1]) %>% 
                  mutate(
                    landings      = round(as.vector(rowSums(h2.mod[[1]]$data$Fcaton)), 0),
                    ssb       = round(h2.mod[[1]]$output[[2]]$SSB[y_ind,2], 0),
                    r         = round(h2.mod[[1]]$output[[2]]$R[,2], 0),
                    fmort     = round(h2.mod[[1]]$output[[2]]$msy_mt[,6], 2),
                    fmsy      = round(h2.mod[[1]]$output[[2]]$msy_mt[,5], 2),
                    ssbmsy    = round(h2.mod[[1]]$output[[2]]$msy_mt[,10], 0),
                    )
res.h2.2


#########
# To get fished over unfished total biomass
#########
h1.uf.b <- h1.mod[[1]]$output[[1]]$TotBiom_NoFish[dim(h1.mod[[1]]$output[[1]]$TotBiom_NoFish)[1],2]
h1.f.b <- h1.mod[[1]]$output[[1]]$TotBiom[dim(h1.mod[[1]]$output[[1]]$TotBiom)[1],2]

h1.f.b / h1.uf.b

h2.1.uf.b <- h2.mod[[1]]$output[[1]]$TotBiom_NoFish[dim(h2.mod[[1]]$output[[1]]$TotBiom_NoFish)[1],2]
h2.1.f.b <- h2.mod[[1]]$output[[1]]$TotBiom[dim(h2.mod[[1]]$output[[1]]$TotBiom)[1],2]

h2.1.f.b / h2.1.uf.b

h2.2.uf.b <- h2.mod[[1]]$output[[2]]$TotBiom_NoFish[dim(h2.mod[[1]]$output[[2]]$TotBiom_NoFish)[1],2]
h2.2.f.b <- h2.mod[[1]]$output[[2]]$TotBiom[dim(h2.mod[[1]]$output[[2]]$TotBiom)[1],2]

h2.2.f.b / h2.2.uf.b

#########
# Current SSB (year, SSB, SSB.sd, lower bound, upper bound)
#########
h1.mod[[1]]$output[[1]]$SSB

h2.mod[[1]]$output[[1]]$SSB
h2.mod[[1]]$output[[2]]$SSB


######
# Estimated b/bmsy
# Yr Fspr 1-Fspr F/Fmsy Fmsy F Fsprmsy MSY MSYL Bmsy Bzero SSB B/Bmsy
######
h1.mod[[1]]$output[[1]]$msy_mt

h2.mod[[1]]$output[[1]]$msy_mt
h2.mod[[1]]$output[[2]]$msy_mt