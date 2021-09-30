
library(jjmR)

#--------------------------------------------------------
# Working directory should be in assessment folder of jjm
#--------------------------------------------------------
# setwd(file.path(getwd(), "assessment"))
pwd <- getwd()
if (!grepl(basename(pwd), "assessment", ignore.case = TRUE)) {
  stop(paste("Set working directory to jjm/assessment"))
}
geth <- function(mod,h=hyp) paste0(h,"_", mod) # Package? Or keep?

# Function to put a constant reference point into image
fixed_bmsy <- function(mod,refpt=5500){
  old_rat <- (mod[[1]]$output[[1]]$msy_mt[,13])
  new_rat <- (mod[[1]]$output[[1]]$msy_mt[,12]/ refpt)
  mod[[1]]$output[[1]]$msy_mt[,13] <- new_rat
  mod[[1]]$output[[1]]$msy_mt[,10] <- refpt
  return(mod)
}

fn.update <- function(newmod, newmodname, h) {

  names(newmod) <- newmod[[1]]$control$modelName <- geth(newmodname,h)
  newmod[[1]]$control$dataFile <- paste0(newmodname,".dat")
  
  writeJJM(newmod,datPath="input",ctlPath="config")
}

#---------

FinModName <- "1.05"
FinMod.h1 <- readJJM(geth(FinModName,"h1"),path="config",input="input")
FinMod.h2 <- readJJM(geth(FinModName,"h2"),path="config",input="input")


#---------
h1_modhl <- FinMod.h1
h1_modhl[[1]]$control$Steepness[1,1] <- .8
h1_modhl[[1]]$control$Nyrs_sr <- 46
h1_modhl[[1]]$control$Nyrs_sr_1 <- 1970:2015

h1_modhs <- FinMod.h1
h1_modhs[[1]]$control$Steepness[1,1] <- .8
h1_modhs[[1]]$control$Nyrs_sr <- 16
h1_modhs[[1]]$control$Nyrs_sr_1 <- 2000:2015

h1_modll <- FinMod.h1
h1_modll[[1]]$control$Steepness[1,1] <- .65
h1_modll[[1]]$control$Nyrs_sr <- 46
h1_modll[[1]]$control$Nyrs_sr_1 <- 1970:2015

h1_modls <- FinMod.h1
h1_modls[[1]]$control$Steepness[1,1] <- .65
h1_modls[[1]]$control$Nyrs_sr <- 16
h1_modls[[1]]$control$Nyrs_sr_1 <- 2000:2015

# fn.update(h1_modhl, paste0(FinModName,".hl"),"h1")
# fn.update(h1_modll, paste0(FinModName,".ll"),"h1")
# fn.update(h1_modhs, paste0(FinModName,".hs"),"h1")
# fn.update(h1_modls, paste0(FinModName,".ls"),"h1")
# modhl  <- runit(geth(paste0(FinModName,".hl"),"h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modll  <- runit(geth(paste0(FinModName,".ll"),"h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modhs  <- runit(geth(paste0(FinModName,".hs"),"h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modls  <- runit(geth(paste0(FinModName,".ls"),"h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

# Only do this for h1
# Should not be setting BMSY for h2
# Most of this is setting BMSY at the interim level

h1_modhl <- readJJM(geth(paste0(FinModName,".hl"),"h1"), path = "config", input = "input")
h1_modhs <- readJJM(geth(paste0(FinModName,".hs"),"h1"), path = "config", input = "input")
h1_modll <- readJJM(geth(paste0(FinModName,".ll"),"h1"), path = "config", input = "input")
h1_modls <- readJJM(geth(paste0(FinModName,".ls"),"h1"), path = "config", input = "input")

h1_modls[[1]]$output[[1]]$msy_mt[,13] <- h1_modls[[1]]$output[[1]]$msy_mt[,12]/ 5500
h1_modls[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(h1_modls, format="word", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))
report(h1_modls, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25),tangle=T)

h1_modhs[[1]]$output[[1]]$msy_mt[,13] <- h1_modhs[[1]]$output[[1]]$msy_mt[,12]/ 5500
h1_modhs[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(h1_modhs, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

h1_modhl[[1]]$output[[1]]$msy_mt[,13] <- h1_modhl[[1]]$output[[1]]$msy_mt[,12]/ 5500
h1_modhl[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(h1_modhl, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

h1_modll[[1]]$output[[1]]$msy_mt[,13] <- h1_modll[[1]]$output[[1]]$msy_mt[,12]/ 5500
h1_modll[[1]]$output[[1]]$msy_mt[,10] <- 5500
report(h1_modll, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

kobe(h1_modls)
FinMod.h1_msy <- fixed_bmsy(FinMod.h1)
FinMod.h1_msy
kobe(FinMod.h1_msy)

report(FinMod.h1_msy, format="word", output="risk_tables/")

# 20 year projection table
summary(FinMod.h1_msy, Projections=TRUE, Fmult=c(0, "FMSY", .75, 1, 1.25))

#--------------

h2_modhl <- FinMod.h2
h2_modhl[[1]]$control$Steepness[1,1:3] <- .8
h2_modhl[[1]]$control$Nyrs_sr[1] <- 46
h2_modhl[[1]]$control$Nyrs_sr_1 <- 1970:2015

h2_modhs <- FinMod.h2
h2_modhs[[1]]$control$Steepness[1,1:3] <- .8
h2_modhs[[1]]$control$Nyrs_sr[1] <- 16
h2_modhs[[1]]$control$Nyrs_sr_1 <- 2000:2015

h2_modll <- FinMod.h2
h2_modll[[1]]$control$Steepness[1,1:3] <- .65
h2_modll[[1]]$control$Nyrs_sr[1] <- 46
h2_modll[[1]]$control$Nyrs_sr_1 <- 1970:2015

h2_modls <- FinMod.h2
h2_modls[[1]]$control$Steepness[1,1:3] <- .65
h2_modls[[1]]$control$Nyrs_sr[1] <- 16
h2_modls[[1]]$control$Nyrs_sr_1 <- 2000:2015

# fn.update(h2_modhl, paste0(FinModName,".hl"),"h2")
# fn.update(h2_modll, paste0(FinModName,".ll"),"h2")
# fn.update(h2_modhs, paste0(FinModName,".hs"),"h2")
# fn.update(h2_modls, paste0(FinModName,".ls"),"h2")
# modhl  <- runit(geth(paste0(FinModName,".hl"),"h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modll  <- runit(geth(paste0(FinModName,".ll"),"h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modhs  <- runit(geth(paste0(FinModName,".hs"),"h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
# modls  <- runit(geth(paste0(FinModName,".ls"),"h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

h2_modhl <- readJJM(geth(paste0(FinModName,".hl"),"h2"), path = "config", input = "input")
h2_modhs <- readJJM(geth(paste0(FinModName,".hs"),"h2"), path = "config", input = "input")
h2_modll <- readJJM(geth(paste0(FinModName,".ll"),"h2"), path = "config", input = "input")
h2_modls <- readJJM(geth(paste0(FinModName,".ls"),"h2"), path = "config", input = "input")

report(h2_modls, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))
report(h2_modls, format="word", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

report(h2_modhs, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

report(h2_modhl, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

report(h2_modll, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

report(FinMod.h2, format="word", output="risk_tables/")

kobe(h2_modls)

# 20 year projection table
summary(h2_modls, Projections=TRUE, Fmult=c(0, "FMSY", .75, 1, 1.25),plot=F)

