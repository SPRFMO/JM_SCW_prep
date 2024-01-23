## head ------------------------------------------------------

rm(list = ls(all = TRUE))

library(jjmR)
library(tidyverse)
library(magrittr)
library(reshape2)
library(ggthemes)
library(dplyr)

setwd(getwd())

col.cohort <- function(agep){
  if (agep < min(agep) + 24) {
    tmp = agep
  } else if (agep >= min(agep) + 24 & agep < min(agep) + 48) {
    tmp = agep - 24
  }
  else
    tmp = agep - 48
}

## 
## read models ------------------------------------------------------
mod0.0            <- readJJM("mod0.0", path="config", input="input")
mod0.1            <- readJJM("mod0.1", path="config", input="input")
mod0.2            <- readJJM("mod0.2", path="config", input="input")
mod0.3            <- readJJM("mod0.3", path="config", input="input")
mod0.4            <- readJJM("mod0.4", path="config", input="input")
mod0.5            <- readJJM("mod0.5", path="config", input="input")
mod0.6            <- readJJM("mod0.6", path="config", input="input")
mod0.7            <- readJJM("mod0.7", path="config", input="input")

mod1.0            <- readJJM("mod1.0", path="config", input="input")
mod1.1            <- readJJM("mod1.1", path="config", input="input")


report(mod0.0, format="pdf", output="risk_tables/")
report(mod0.1, format="pdf", output="risk_tables/")
report(mod0.2, format="pdf", output="risk_tables/")
report(mod0.3, format="pdf", output="risk_tables/")
report(mod0.4, format="pdf", output="risk_tables/")
report(mod0.5, format="pdf", output="risk_tables/")
report(mod0.6, format="pdf", output="risk_tables/")
report(mod0.7, format="pdf", output="risk_tables/")

report(mod1.0, format="pdf", output="risk_tables/")
report(mod1.1, format="pdf", output="risk_tables/")


