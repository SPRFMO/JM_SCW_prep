
library(jjmR)
library(foreach)
library(doParallel)

ncores <- 2
cl <- if(Sys.info()[["sysname"]]=="Windows") makePSOCKcluster(ncores) else ncores
registerDoParallel(cl)

fn_update <- function(newmod, newmodname, h, dodat=F) {

  names(newmod) <- newmod[[1]]$control$modelName <- geth(newmodname,h)

  if(dodat) newmod[[1]]$control$dataFile <- paste0(newmodname,".dat")

  writeJJM(newmod,datPath="input",ctlPath="config")
}

#--------------------------------------------------------
# Working directory should be in assessment folder of jjm
#---------

finmodname <- "1.14"
finmod_h1 <- readJJM(geth(finmodname,"h1"),path="config",input="input")
finmod_h2 <- readJJM(geth(finmodname,"h2"),path="config",input="input")

tac_prev <- 1552500/1000 # From CMM 01-2025

mod_tac <- runit(geth(finmodname,c("h1","h2")), pdf=T ,portrait=F ,est=TRUE ,exec="../src/jjm",parallel=T, adflags=paste0("-tac ", tac_prev))

#---------
h1_modls <- finmod_h1
h2_modls <- finmod_h2

tac_prev <- sum(tail(finmod_h1[[1]]$data$Fcaton,1))

h1_modls[[1]]$control$Steepness[1,1] <- h2_modls[[1]]$control$Steepness[1,1:3] <- .65
h1_modls[[1]]$control$Nyrs_sr <- h2_modls[[1]]$control$Nyrs_sr[1] <- 15
h1_modls[[1]]$control$Nyrs_sr_1 <- h2_modls[[1]]$control$Nyrs_sr_1 <- 2001:2015

fn_update(h1_modls, paste0(finmodname,".ls"),"h1")
fn_update(h2_modls, paste0(finmodname,".ls"),"h2")
modls  <- runit(geth(paste0(finmodname,".ls"),c("h1","h2")),parallel=TRUE,pdf=TRUE,portrait=F,est=T,exec="../src/jjm", adflags=paste0("-tac ", tac_prev))
# modls  <- runit(geth(paste0(finmodname,".ls"),c("h1","h2")),parallel=TRUE,pdf=F,portrait=F,est=T,exec="../src/jjm")

# "jjm -ind … -binp jjm.bar -phase 22 -tac 1242 -sdonly"

# Most of this is setting BMSY at the average level for the last ten years.

h1_modls <- readJJM(geth(paste0(finmodname,".ls"),"h1"), path = "config", input = "input")

h1_msy <- fixed_bmsy(finmod_h1)
bmsy_h1 <- unique(get_msy_mt(h1_msy)$bmsy)

h1_ls_msy <- fixed_bmsy(h1_modls, refpt = bmsy_h1)

# fut_6 is F=0
# fut_5 is TAC
report(h1_msy,    format="word", output="risk_tables/", Fmult=c(0, .75, 1, 1.25, "FMSY", "FTAC"))
report(h1_ls_msy, format="word", output="risk_tables/", Fmult=c(0, .75, 1, 1.25, "FMSY", "FTAC"))
report(h1_ls_msy, format="pdf",  output="risk_tables/", Fmult=c(0, .75, 1, 1.25, "FMSY", "FTAC"))

# 20 year projection table
summary(h1_msy, Projections=TRUE, Fmult=c(0, "FMSY", .75, 1, 1.25, "FTAC"), plot = F)

#---

h2_modls <- readJJM(geth(paste0(finmodname,".ls"),"h2"), path = "config", input = "input")

h2_msy <- fixed_bmsy(finmod_h2)
bmsy_h2 <- unique(get_msy_mt(h2_msy)$bmsy)

h2_ls_msy <- fixed_bmsy(h2_modls, refpt = bmsy_h2)

report(h2_msy, format="word", output="risk_tables/", Fmult=c(0, .75, 1, 1.25, "FMSY", "FTAC"))
report(h2_ls_msy, format="word", output="risk_tables/", Fmult=c(0, .75, 1, 1.25, "FMSY", "FTAC"))
report(h2_ls_msy, format="pdf", output="risk_tables/", Fmult=c(0, .75, 1, 1.25, "FMSY", "FTAC"))

# 20 year projection table
summary(h2_ls_msy, Projections=TRUE, Fmult=c(0, "FMSY", .75, 1, 1.25, "FTAC"),plot=F)


# modls  <- runit(geth(paste0(finmodname,".ls"),c("h1","h2")),parallel=TRUE,pdf=TRUE,portrait=F,est=T,exec="../src/jjm", adflags=paste0("-tac ", tac_prev))
