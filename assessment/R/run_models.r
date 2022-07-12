# Basic script to run a model.
# Assumes you're in the jjm/assessment directory.
library(jjmR)

geth <- function(mod,h=hyp) paste0(h,"_", mod)

modnm <- "0.04"

tmp1 <- runit(geth(modnm,"h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

tmp2 <- runit(geth(modnm,"h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

