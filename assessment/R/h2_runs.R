
library(jjmR)

#--------------------------------------------------------
# Working directory should be in assessment folder of jjm
#---------

finmodname <- "1.14"
tac_prev <- 1428 # From SC recommended quota
tac_new <- 1428*1.15 # From SC recommended quota

mod_tac <- runit("h2_1.14", pdf=T ,portrait=F ,est=T ,exec="../src/jjm",adflags=paste0("-tac ", tac_prev, " -fut_sel 3"))

modls  <- runit("h2_1.14.ls",pdf=T, portrait=F,est=T,exec="../src/jjm", adflags=paste0("-tac ", tac_new, " -fut_sel 3"))

# If the models don't run, I would move the model files to two separate folders

# For model h2_1.14
# For the first run:
# jjm -ind h2_1.14.ctl -tac 1428 -fut_sel 3
# If it doesn't converge:
# jjm -ind h2_1.14.ctl -tac 1428 -fut_sel 3 -binp jjm.b02


# For model h2_1.14.ls
# For the first run:
# jjm -ind h2_1.14.ls.ctl -tac 1642 -fut_sel 3
# If it doesn't converge:
# jjm -ind h2_1.14.ls.ctl -tac 1642 -fut_sel 3 -binp jjm.b02


# From each run I would need 7 files
# - Fprof.yld
# - For_R_1.rep
# - For_R_2.rep
# - jjm.cor
# - jjm.par
# - jjm.rep
# - jjmpstd
