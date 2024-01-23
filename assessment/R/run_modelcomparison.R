# Script to run model comparisons
# Assumes you're in jjm/assessment directory.
library(jjmR)

geth <- function(mod,h=hyp) paste0(h,"_", mod)

# If you haven't read your models in yet
mods2compare <- c("1.10","1.11")
mods <- compareModels(geth(mods2compare, "h1"))

# If you already have models read in as readJJM objects
# mod1 <- readJJM(geth("0.00","h1"), path = "config", input = "input")
# mod2 <- readJJM(geth("1.11","h1"), path = "config", input = "input")
# mods <- combineModels(mod1, mod2)

plot(mods,combine=T,what="biomass",stack=F,main="Biomass")
plot(mods,combine=T,what="recruitment",stack=F,main="Recruitment")
plot(mods,combine=T,what="ftot",stack=F,main="Total Fishing Mortality")

summary(mods)$like
