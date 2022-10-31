
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
fixed_bmsy <- function(mod, refpt=T){
  if(refpt) refpt <- mean(rev(mod[[1]]$output[[1]]$msy_mt[,10])[1:10])
  old_rat <- (mod[[1]]$output[[1]]$msy_mt[,13])
  new_rat <- (mod[[1]]$output[[1]]$msy_mt[,12]/ refpt)
  mod[[1]]$output[[1]]$msy_mt[,13] <- new_rat
  mod[[1]]$output[[1]]$msy_mt[,10] <- refpt
  return(mod)
}

fn.update <- function(newmod, newmodname, h) {

  names(newmod) <- newmod[[1]]$control$modelName <- geth(newmodname,h)
#  newmod[[1]]$control$dataFile <- paste0(newmodname,".dat") # Double-check if this is actually necessary
  
  writeJJM(newmod,datPath="input",ctlPath="config")
}

#---------

FinModName <- "1.02"
FinMod_h1 <- readJJM(geth(FinModName,"h1"),path="config",input="input")
FinMod_h2 <- readJJM(geth(FinModName,"h2"),path="config",input="input")


#---------
h1_modls <- FinMod_h1
h1_modls[[1]]$control$Steepness[1,1] <- .65
h1_modls[[1]]$control$Nyrs_sr <- 16
h1_modls[[1]]$control$Nyrs_sr_1 <- 2000:2015

# fn.update(h1_modls, paste0(FinModName,".ls"),"h1")
# modls  <- runit(geth(paste0(FinModName,".ls"),"h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

# Only do this for h1
# Should not be setting BMSY for h2
# Most of this is setting BMSY at the average level for the last ten years.

h1_modls <- readJJM(geth(paste0(FinModName,".ls"),"h1"), path = "config", input = "input")  %>% fixed_bmsy()

report(h1_modls, format="word", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))
report(h1_modls, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

kobe(h1_modls)
FinMod_h1_msy <- fixed_bmsy(FinMod_h1)
kobe(FinMod_h1_msy)

report(FinMod_h1_msy, format="word", output="risk_tables/")

# 20 year projection table
summary(FinMod_h1_msy, Projections=TRUE, Fmult=c(0, "FMSY", .75, 1, 1.25))

#--------------

h2_modls <- FinMod_h2
h2_modls[[1]]$control$Steepness[1,1:3] <- .65
h2_modls[[1]]$control$Nyrs_sr[1] <- 16
h2_modls[[1]]$control$Nyrs_sr_1 <- 2000:2015

# fn.update(h2_modls, paste0(FinModName,".ls"),"h2")
# modls  <- runit(geth(paste0(FinModName,".ls"),"h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

h2_modls <- readJJM(geth(paste0(FinModName,".ls"),"h2"), path = "config", input = "input")

report(h2_modls, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))
report(h2_modls, format="word", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

report(FinMod_h2, format="word", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

kobe(h2_modls)

# 20 year projection table
summary(h2_modls, Projections=TRUE, Fmult=c(0, "FMSY", .75, 1, 1.25),plot=F)

