# ------------------------------------------------------------------------
# Script for SC04------------------------------------------------------------
# ------------------------------------------------------------------------
#install.packages("devtools")
# library(devtools)
# devtools::install_github("sprfmo/jjmr")
library(jjmR)

# runJJM("mod2015", path="config", input="input", exec="jjms")
mod2015 <- readJJM("mod2015", path="config", input="input", version="2014")
mod0.0 <- mod2015
names(mod0.0) = "mod1.13"
ret1 = retro(model = mod0.0, n = 2, output = "results", exec="jjms")
plot(ret1) # all plots
plot(ret1, var="SSB") # only SSB
plot(ret1, var="SSB", std=TRUE) # standadization 





