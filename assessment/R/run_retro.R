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

geth <- function(mod,h=hyp) paste0(h,"_", mod)


modnm <- "1.11"

h1nm <- geth(modnm,"h1")
mod_h1 <- readJJM(h1nm, path = "config", input = "input")
mod_h1r <- mod_h1
names(mod_h1r) <- h1nm
Npeels<-5

# ret1 <- retro(model = mod_h1r, n = Npeels, output = "results", exec="../src/jjms",parallel=T)

load(paste0("results/",h1nm,"_retrospective.RData"))
ret1<-output
pdf(paste0("results/",h1nm,"_Retro.pdf"))
plot(ret1) # all plots
dev.off()

plot(ret1, var="SSB") # only SSB
#Calculation of Mohn's Rho courtesy of Arni Magnusson
icesAdvice::mohn(ret1$Stock_1$SSB$var[,1,1:6],peel=5,details=T)

#----

h2nm <- geth(modnm,"h2")
mod_h2 <- readJJM(h2nm, path = "config", input = "input")
mod_h2r <- mod_h2
names(mod_h2r) <- h2nm
Npeels<-5

# ret2 <- retro(model = mod_h2r, n = Npeels, output = "results", exec="../src/jjms",parallel=T)

load(paste0("results/",h2nm,"_retrospective.RData"))
ret2<-output
pdf(paste0("results/",h2nm,"_Retro.pdf"))
plot(ret2) # all plots
dev.off()

plot(ret2, var="SSB") # only SSB
#install.packages("icesAdvice")
#Calculation of Mohn's Rho courtesy of Arni Magnusson
icesAdvice::mohn(ret2$Stock_1$SSB$var[,1,1:6],peel=5,details=T)
icesAdvice::mohn(ret2$Stock_2$SSB$var[,1,1:6],peel=5,details=T)
