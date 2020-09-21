#install.packages("devtools") library(devtools) devtools::install_github("sprfmo/jjmr")
library(jjmR)
library(data.table); library(ggplot2); source("theme_fav.r")

setwd("C:/Users/leeqi/Documents/Github/jjm/assessment")
setwd("~/_mymods/sprfmo/jjm/assessment")
dir.main <- getwd() # ~/jjm/assessment
modnm <- "mod1.11"
retmodnm <- "model_1.11_r0"
file.dat <- "mod1.0.dat"

retmodnm <- "model_2.14_r0"
modnm <- "mod2.14"
file.dat <- "mod1.3.dat"

runJJM( modnm,  path="config", input="input", version="2014",exec="jjms")
modtmp <- readJJM(modnm, path="config", input="input", version="2014")
names(modtmp) <- modnm
dir.mod
library(snowfall)
nCores = 7
cl = makeCluster(rep("localhost",times=nCores), type = "SOCK")
registerDoSNOW(cl)
retres <- retro(modtmp, n = 9,parallel=TRUE, temp = "retro", exec = "jjms")
stopCluster(cl)
i=1
for(i in 1:9) {

dir.mod <- paste0(retmodnm,i)
setwd(file.path(dir.main,"retro",dir.mod))
dir.mod
file.ctl <- paste0(dir.mod, ".ctl")
file.copy(from = file.ctl, to = file.path(dir.main, "config", paste(dir.mod, ".ctl", sep = "")))
file.copy(from = file.dat, to = file.path(dir.main, "input", paste(dir.mod, ".dat", sep = "")))

file.copy(from="jjms.par",   to=file.path(dir.main,  "results", paste(dir.mod, ".par", sep = "")), overwrite = TRUE)
file.copy(from="jjms.rep",   to=file.path(dir.main,  "results", paste(dir.mod, ".rep", sep = "")), overwrite = TRUE)
file.copy(from="jjms.std",   to=file.path(dir.main,  "results", paste(dir.mod, ".std", sep = "")), overwrite = TRUE)
file.copy(from="jjms.cor",   to=file.path(dir.main,  "results", paste(dir.mod, ".cor", sep = "")), overwrite = TRUE)
file.copy(from="Fprof.yld", to=file.path(dir.main, "results", paste(dir.mod, ".yld", sep = "")), overwrite = TRUE)

Files = list.files(pattern = "_R_?[0-9]*\\.rep")
for(j in seq_along(Files)) {
file.copy(from = Files[j], 
  to=file.path(dir.main, "results",paste(dir.mod,"_", j, "_R.rep", sep = "")), overwrite = TRUE)
  } 
}

#
modnm <- "mod1.11"
retmodnm <- "model_1.11_r0"
modnm <- "mod1.14"
retmodnm <- "model_1.14_r0"
modnm <- "mod2.14"
retmodnm <- "model_2.14_r0"
setwd(dir.main)
mn0 <- readJJM(modnm, path="config", input="input", version="2014")
mn1 <- readJJM(paste0(retmodnm,"1"), path="config", input="input", version="2014")
mn2 <- readJJM(paste0(retmodnm,"2"), path="config", input="input", version="2014")
mn3 <- readJJM(paste0(retmodnm,"3"), path="config", input="input", version="2014")
mn4 <- readJJM(paste0(retmodnm,"4"), path="config", input="input", version="2014")
mn5 <- readJJM(paste0(retmodnm,"5"), path="config", input="input", version="2014")
mn6 <- readJJM(paste0(retmodnm,"6"), path="config", input="input", version="2014")
mn7 <- readJJM(paste0(retmodnm,"7"), path="config", input="input", version="2014")
mn8 <- readJJM(paste0(retmodnm,"8"), path="config", input="input", version="2014")
mn9 <- readJJM(paste0(retmodnm,"9"), path="config", input="input", version="2014")

mncomb <- combineModels(mn0, mn1, mn2, mn3, mn4, mn5, mn6, mn7, mn8, mn9)
i=1
fl <- length(mncomb[[i]]$output$Stock_1$SSB[,1])
df <- data.table( mncomb[[i]]$output$Stock_1$SSB[1:fl,], retro=rep(i,fl))
names(df) <- c("yr", "ssb","ssb.sd","ssb.lb","ssb.ub","run" )
for (i in 2:9) {
	fl <- length(mncomb[[i]]$output$Stock_1$SSB[,1])
  dt <- data.table( mncomb[[i]]$output$Stock_1$SSB[1:fl,] , retro=rep(i,fl))
	print(fl)
	df <- rbind( df ,dt,use.names=FALSE)
}
df$run <- as.factor(df$run)
p <- ggplot(df,aes(x=yr,y=ssb,fill=run)) + geom_ribbon(aes(x=yr,ymin=ssb.lb,ymax=ssb.ub,alpha=.12 )) + ylim(c(0,1.5e4)) + xlim(c(2000,2016))
p <- p + geom_line(aes(col=run)) + mytheme + ylab("Spawning biomass (kt)") + xlab("Year")  + theme(legend.position="none") + ggtitle(modnm)
p

i=1
fl <- length(mncomb[[i]]$output$Stock_1$R[,1])
df <- data.table( mncomb[[i]]$output$Stock_1$R, retro=rep(i,fl))
dim(df)
for (i in 2:9) {
	fl <- length(mncomb[[i]]$output$Stock_1$R[,1])
  dt <- data.table( mncomb[[i]]$output$Stock_1$R,  retro=rep(i,fl))
	print(fl)
	df <- rbind( df ,dt,use.names=FALSE)
}
names(df) <- c("yr", "R","R.sd","R.lb","R.ub","run" )
df$run <- as.factor(df$run)

p <- ggplot(df,aes(x=yr,y=R,fill=run)) + geom_ribbon(aes(x=yr,ymin=R.lb,ymax=R.ub,alpha=.12 )) + xlim(c(2000,2016)) + ylim(c(0,100000))
p <- p + geom_line(aes(col=run)) + mytheme + ylab("Recruitment") + xlab("Year")  + theme(legend.position="none") + ggtitle(modnm)
p



