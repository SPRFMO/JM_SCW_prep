#-------------------------------------------
# mod1.0_r: Retrospective analysis on mod1.0
# Code won't work unless go into jjmR directory
# and do devtools::load_all()
#-------------------------------------------

#install.packages("icesAdvice")

dir.jjmR <- "../../jjmR"

dir.jjm <- getwd()
library(foreach)
library(doParallel)
registerDoParallel(5)
setwd(dir.jjmR)
devtools::load_all()
setwd(dir.jjm)


modnm <- "h2_1.02"

mod <- readJJM(modnm, path = "config", input = "input")
mod_r <- mod
names(mod_r) <- modnm
Npeels<-5

ret1 <- retro(model = mod_r, n = Npeels, output = "results", exec="../src/jjms",parallel=T)

load(paste0("results/",modnm,"_retrospective.RData"))
ret<-output
pdf(paste0("results/",modnm,"_Retro.pdf"))
plot(ret) # all plots
dev.off()

plot(ret, var="SSB") # only SSB
#Calculation of Mohn's Rho courtesy of Arni Magnusson
icesAdvice::mohn(ret$Stock_1$SSB$var[,1,1:6],peel=5,details=T)
