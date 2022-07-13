# Expanded from Renzo Tascheri's code to pull Francis weights into a table
# Should theoretically work, but untested as of 12/07/21
# Lee Qi
# leeqi@uw.edu

library(jjmR)
library(tidyverse)
modnm <- "1.11"
newmodnm <- "1.12"

geth <- function(mod,h=hyp) paste0(h,"_", mod) 

fn_bridge <- function(newmod, newmodname, h2mod=h2_ctl,ln_dat=line_dat, ln_modnm=line_modnm) {

  names(newmod) <- newmod[[1]]$control$modelName <- geth(newmodname,"h1")
  newmod[[1]]$control$dataFile <- h2mod[ln_dat] <- paste0(newmodname,".dat")

  h2mod[ln_dat] <- paste0(newmodname, ".dat")
  h2mod[ln_modnm] <- geth(newmodname, "h2")
  
  writeJJM(newmod,datPath="input",ctlPath="config")
  writeLines(h2mod, con=paste0("config/h2_",newmodname,".ctl"))
}

# tmp1 <- runit(paste0("h1_",modnm),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

mod <- readJJM(geth(modnm,"h1"), path = "config", input = "input")
ind <- grep("FW_",names(mod[[1]]$output[[1]]))

W_labels <- names(mod[[1]]$output[[1]][ind])
ind_names <- str_sub(W_labels, 4)

Ws <- mod[[1]]$output[[1]][ind] %>% unlist()

Find <- which(mod[[1]]$data$Fnames %in% ind_names)
Iind <- which(mod[[1]]$data$Inames %in% ind_names & mod[[1]]$data$Inames!="DEPM")

newmod <- mod

newmod[[1]]$data$Fagesample[,Find] <- apply(mod[[1]]$data$Fagesample[,Find], 1, function(x) x * Ws[ind_names %in% mod[[1]]$data$Fnames]) %>% t()
newmod[[1]]$data$Iagesample[,Iind] <- apply(mod[[1]]$data$Iagesample[,Iind], 1, function(x) x * Ws[ind_names %in% mod[[1]]$data$Inames & ind_names!="DEPM"]) %>% t()

h2_ctl <- readLines(paste0("config/h2_",modnm,".ctl"))
line_dat <- grep("dataFile", h2_ctl) + 1
line_modnm <- grep("modelName", h2_ctl) + 1

fn_bridge(newmod, newmodnm)

# tmp1 <- runit(paste0("h1_",newmodnm),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# tmp2 <- runit(paste0("h2_",newmodnm),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

#----
# Table for Francis Weights for more than one model (but no more than 5, for some reason)

mods2compare <- c("h1_1.11","h2_1.11","h1_1.12","h2_1.12")
comp.casos <-  compareModels(mods2compare)
W_labels <- names(comp.casos[[1]]$output$Stock_1[c(3:8)]); W_labels
cases.names <-  mods2compare; cases.names
##
for(i in 1:length(mods2compare)){
    ##    
    if(i==1){ Ws <- comp.casos[[i]]$output$Stock_1[c(3:8)]}
    else {Ws<- cbind(Ws, comp.casos[[i]]$output$Stock_1[c(3:8)])}
    ##
}
##
weights.table  <- data.frame(Ws)
colnames(weights.table) <- cases.names
rownames(weights.table)  <-  W_labels
weights.table