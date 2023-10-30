
library(jjmR)

#--------------------------------------------------------
# Working directory should be in assessment folder of jjm

#---------

finmodname <- "1.07"
finmod_h1 <- readJJM(geth(finmodname,"h1"),path="config",input="input")
finmod_h2 <- readJJM(geth(finmodname,"h2"),path="config",input="input")


#---------
h1_modls <- finmod_h1
h1_modls[[1]]$control$Steepness[1,1] <- .65
h1_modls[[1]]$control$Nyrs_sr <- 15
h1_modls[[1]]$control$Nyrs_sr_1 <- 2001:2015

# fn_update(h1_modls, paste0(finmodname,".ls"),"h1")
# modls  <- runit(geth(paste0(finmodname,".ls"),"h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")
# "jjm -ind … -binp jjm.bar -phase 22 -tac 1242 -sdonly"

# Only do this for h1
# Should not be setting BMSY for h2
# Most of this is setting BMSY at the average level for the last ten years.

h1_modls <- readJJM(geth(paste0(finmodname,".ls"),"h1"), path = "config", input = "input")  %>% fixed_bmsy()

# report(h1_modls, format="word", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))
# report(h1_modls, format="pdf", output="risk_tables/",Fmult=c(0, "FMSY", .75, 1, 1.25))

kobe(h1_modls)
h1_msy <- fixed_bmsy(finmod_h1)
kobe(h1_msy)

h1_ls_msy <- fixed_bmsy(h1_modls)

report(h1_msy, format="word", output="risk_tables/", Fmult=c(0, "FMSY", .75, 1, 1.25))
report(h1_ls_msy, format="word", output="risk_tables/", Fmult=c(0, "FMSY", .75, 1, 1.25))
report(h1_ls_msy, format="pdf", output="risk_tables/", Fmult=c(0, "FMSY", .75, 1, 1.25))

# 20 year projection table
summary(finmod_h1_msy, Projections=TRUE, Fmult=c(0, "FMSY", .75, 1, 1.25))

#--------------

h2_modls <- finmod_h2
h2_modls[[1]]$control$Steepness[1,1:3] <- .65
h2_modls[[1]]$control$Nyrs_sr[1] <- 15
h2_modls[[1]]$control$Nyrs_sr_1 <- 2001:2015

# fn_update(h2_modls, paste0(finmodname,".ls"),"h2")
# modls  <- runit(geth(paste0(finmodname,".ls"),"h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjm")

h2_modls <- readJJM(geth(paste0(finmodname,".ls"),"h2"), path = "config", input = "input")

kobe(h2_modls)
h2_msy <- fixed_bmsy(finmod_h2)
kobe(h2_msy)

h2_ls_msy <- fixed_bmsy(h2_modls)

report(h2_msy, format="word", output="risk_tables/", Fmult=c(0, "FMSY", .75, 1, 1.25))
report(h2_ls_msy, format="word", output="risk_tables/", Fmult=c(0, "FMSY", .75, 1, 1.25))
report(h2_ls_msy, format="pdf", output="risk_tables/", Fmult=c(0, "FMSY", .75, 1, 1.25))


# 20 year projection table
summary(h2_ls_msy, Projections=TRUE, Fmult=c(0, "FMSY", .75, 1, 1.25),plot=F)

