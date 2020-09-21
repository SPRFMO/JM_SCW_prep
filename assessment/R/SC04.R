# ------------------------------------------------------------------------
# Script for SC04------------------------------------------------------------
# ------------------------------------------------------------------------
#install.packages("devtools")
#library(devtools)
#devtools::install_github("sprfmo/jjmr")
R
library(jjmR)

# Set parameters ----------------------------------------------------------
# Path of JJM repository (from current working directory)
setwd("/Users/jim/_mymods/sprfmo/jjm/assessment")

mod1.11s_1.6n_2stk
mod1.11s_1.6n_2stk
# --- Run models  -----------------------------------------------
source("R/prelims.R")
runit("mod1.18",pdf=TRUE,portrait=F,est=TRUE,exec="jjms")
for (i in 0:3) assign(paste0("mod2.",i),runit(paste0("mod2.",i), est=TRUE,portrait=F,pdf=TRUE))
mod1.18 <-  runit("mod1.19",est=TRUE, portrait=F,pdf=TRUE)
mod1.18 <-  runit("mod1.18",est=TRUE, portrait=F,pdf=TRUE)
mod1.2s <-  runit("mod1.11s_1.6n_2stk",est=TRUE, portrait=F,pdf=TRUE)
mod1.2s <-  runit("mod1.18s_1.6n_2stk",est=TRUE, portrait=F,pdf=TRUE)
# --- Read model results in -----------------------------------------------
report.jjm.diag(mod1.11s_1.6n_2stk )
report.jjm.diag(mod1.11s_1.6n_2stk )
sumList = summary(mod2.0, Projections = T)
print(sumList)
<- readJJM("mod1.11s_1.6n_2stk", path="config", input="input",exec="jjm",version="2014")
mod1.11s_1.6n_2stk <- readJJM("mod1.11s_1.6n_2stk", path="config", input="input",exec="jjm",version="2014")
mod2.0 <- readJJM("mod2.0", path="config", input="input",exec="jjm",version="2014")
mod2.1 <- readJJM("mod2.1", path="config", input="input",exec="jjm",version="2014")
mod2.2 <- readJJM("mod2.2", path="config", input="input",exec="jjm",version="2014")
mod2.3 <- readJJM("mod2.3", path="config", input="input",exec="jjm",version="2014")
mod1.11 <- readJJM("mod1.11", path="config", input="input",exec="jjm",version="2014")

# --- Write to pdf file output --------------------------------
pdf(paste0("results/mod2.3.pdf"),height=7,width=9)
plot(diagnostics(mod2.3))
dev.off()

#--- Likelihood tables for 1.6, 1.17, 1.18, 1.19

#--- Risk tables for 2.0, 2.1 etc.---------------------------------
report(mod2.0,output="results/")

# --- combine them and plot ----------------------------------------
tmp <- combineModels(mod1.11, mod2.0  ,mod2.1,mod2.2,mod2.3)
plot(tmp,combine=T,what="recruitment",stack=F)
plot(tmp,combine=T,what="recruitment",stack=T)
plot(tmp,combine=T,what="biomass",stack=F)


# --- Make a Kobe plot ----------------------------------------
kobe(mod2.0,ylim=c(0,6),xlim=(c(0,2.4)))
kobe(mod2.2, add=TRUE, col="grey")
# --- Make a Kobe plot ----------------------------------------
mod = "mod1.6_2stk"
plot(retres_1.10,var="SSB",ci=FALSE)
plot(retres,var="SSB",ci=FALSE,std=TRUE)
plot(retres,var="SSB",CI=TRUE)
plot(retres,var="F",ci=FALSE) # no ci
plot(retres,var="R")
# ------------------------------------------------------------- 
model <- readJJM(mod, path="config", input="input")
k1=jjmR:::.kobeFUN(model[[1]]$output$Stock_1, xlim=c(0,5), ylim=c(0,8))
plot(k1)
jjmR:::.kobeFUN(model[[1]]$output$Stock_2, pic.add = k1, add = T, xlim=c(0,5), ylim=c(0,8))

# --- Other runs --------------------------------------------
runJJM("mod0.9",  path="config", input="input", exec="jjms")
runJJM("mod0.8",  path="config", input="input", exec="jjms")
runJJM("mod2015", path="config", input="input", exec="jjms")
runJJM("mod1.17", path="config", input="input", exec="jjms")
mod1.14 <- readJJM("mod1.14", path="config", input="input", version="2014")
pdf(paste0("results/mod1.14.pdf"),height=7,width=9)
plot(diagnostics(mod1.14)); dev.off()
mod2015 <- readJJM("mod2015", path="config", input="input", version="2014")
mod0.0 <- mod2015
plot(diagnostics(mod2015))
runJJM("mod0.1", path="config", input="input", exec="jjms")
plot(diagnostics(mod2.0))

runJJM("mod1.14_2stk", path="config", input="input", exec="jjms")

mod0.8 <- readJJM("mod0.8", path="config", input="input", version="2014")

mod1.14_2stk <- readJJM("mod1.14_2stk", path="config", input="input")

names(modtmp$Model_1.13)
names(modtmp$Model_1.13$control)
writeJJM(object = models[[i]]$Dat, outFile = models[[i]]$Ctl$dataFile, path = temp) 

#--------------Retrospective try------------------------------------------------
library(snowfall)
nCores=5
cl = makeCluster(rep("localhost",times=nCores), type = "SOCK")
registerDoSNOW(cl)
#runJJM(models = ModMList, parallel=TRUE, path="config", input="input", version="2014",exec="jjms")
ret2.0 <- retro(mod2.0,parallel=TRUE,n=9,exec="jjms")
#runJJM(models = ModMList, parallel=TRUE, path="config", input="input", version="2014",exec="jjms")
stopCluster(cl)
getwd()
plot(ret2.0,var="SSB",xlim=c(2000,2017),ylim=c(0,10000))

#--- Custom retro plot ---------------------------
dim(ret2.0$Stock_1$SSB$var)
fl <- dim(ret2.0$Stock_1$SSB$var)[1]
i=1
df <- data.table(1957:2017, ret2.0$Stock_1$SSB$var[,,i], retro=rep(i,fl))
names(df) <- c("yr", "ssb","ssb.sd","ssb.lb","ssb.ub","run" )
(ret2.0$Stock_1$SSB$var[,1,i])
for (i in 2:10) {
  dt <- data.table(1957:2017, ret2.0$Stock_1$SSB$var[,,i], retro=rep(i,fl))
  print(fl)
  df <- rbind( df ,dt,use.names=FALSE)
}
df$run <- as.factor(df$run)
p <- ggplot(df,aes(x=yr,y=ssb,fill=run)) + 
geom_ribbon(aes(x=yr,ymin=ssb.lb,ymax=ssb.ub),alpha=.18 ) + 
ylim(c(0,1.0e4)) + xlim(c(2000,2017))
p <- p + geom_line(aes(col=run),size=2) + mytheme + ylab("Spawning biomass (kt)") + xlab("Year")  + theme(legend.position="none") + ggtitle("Mod1.18")
p

fl <- dim(ret2.0$Stock_1$R$var)[1]
df <- data.table(1970:2016, ret2.0$Stock_1$R$var[,,i], retro=rep(i,fl))
names(df) <- c("yr", "ssb","ssb.sd","ssb.lb","ssb.ub","run" )
(ret2.0$Stock_1$SSB$var[,1,i])
for (i in 2:10) {
  dt <- data.table(1970:2016, ret2.0$Stock_1$R$var[,,i], retro=rep(i,fl))
  print(fl)
  df <- rbind( df ,dt,use.names=FALSE)
}
df$run <- as.factor(df$run)
p <- ggplot(df,aes(x=yr,y=ssb,fill=run)) + 
geom_ribbon(aes(x=yr,ymin=ssb.lb,ymax=ssb.ub),alpha=.18 ) + 
ylim(c(0,6.0e4)) + xlim(c(2000,2015))
p <- p + geom_line(aes(col=run),size=2) + mytheme + ylab("Spawning biomass (kt)") + xlab("Year")  + theme(legend.position="none") + ggtitle("Mod1.18")
p





# Run M models in paralle...but doesn't work...parallel that is-------------------------------------------------
ModMList = paste0("mod0.", 8:13)
ModMList = paste0("mod1.", 0:16)
ModMList = paste0("mod2.", 0:3)
ModMList
nCores = 7
mod1.14 <- readJJM("mod1.14", path="config", input="input",exec="jjms")
mod1.11 <- readJJM("mod1.11", path="config", input="input",exec="jjms")
modx.4 <- readJJM("modx.4", path="config", input="input",exec="jjms")
modx.3 <- readJJM("modx.3", path="config", input="input",exec="jjms")
mod1.3 <- readJJM("mod1.3", path="config", input="input",exec="jjms")
mod1.0 <- readJJM("mod1.0", path="config", input="input",exec="jjms")
names(mod1.3)
readJJMConfig("Model_1.3",path="config")
readJJMConfig(mod1.3,control="config",data="input")
jjmR::.getJjmConfig(data = mod1.3, control = mod1.3)
,control="config/mod1.3")
report(modx.4,output="results")
report(mod1.14,output="results")

for (i in 8:13) assign(paste0("mod0.",i),runit(paste0("mod0.",i), est=TRUE, portrait=F,pdf=TRUE))
for (i in 0:13) assign(paste0("mod1.",i),runit(paste0("mod1.",i), est=TRUE,portrait=F,pdf=TRUE))
for (i in 0:7) assign(paste0("mod1.",i),runit(paste0("mod1.",i), portrait=F,pdf=TRUE))
for (i in 10:14) assign(paste0("mod1.",i),runit(paste0("mod1.",i), portrait=F,pdf=TRUE))
ls()
for (i in 1:13) assign(paste0("mod1.",i),runit(paste0("mod1.",i), est=TRUE,portrait=F,pdf=TRUE))
  getwd()
mod2.0 <-  runit("mod2.0", est=TRUE, portrait=F,pdf=TRUE)
mod0.0 <-  runit("mod0.0", portrait=F,pdf=TRUE)
mod0.8 <-  runit("mod0.8",est=TRUE, portrait=F,pdf=TRUE)
mod1.0 <-  runit("mod1.0",est=TRUE, portrait=F,pdf=TRUE)
mod1.14 <-  runit("mod1.14",est=TRUE, portrait=F,pdf=TRUE)
mod1.15 <-  runit("mod1.15",est=TRUE, portrait=F,pdf=TRUE)
mod1.16 <-  runit("mod1.16",est=TRUE, portrait=F,pdf=TRUE)
mod1.17 <-  runit("mod1.17",est=TRUE, portrait=F,pdf=TRUE)
mod2.2  <-  runit("mod2.2",est=TRUE, portrait=F,pdf=TRUE)
modx.3  <-  runit("modx.3",est=TRUE, portrait=F,pdf=TRUE)
modx.4  <-  runit("modx.4",est=TRUE, portrait=F,pdf=TRUE)
modx.5  <-  runit("modx.5",est=TRUE, portrait=F,pdf=TRUE)

modx.2  <-  runit("modx.2",est=TRUE, portrait=F,pdf=TRUE)


#---Likelihood tabling...
lt0 <- summary(mod0.0)$like
for(i in 1:13) lt0 <- cbind(lt0,summary(get(paste0("mod0.",i)))$like)
lt1 <- summary(mod1.0)$like
i=16
lt1 <- cbind(lt1,summary(get(paste0("mod1.",i)))$like)
for(i in c(11,14,15,16))   lt1 <- cbind(lt1,summary(get(paste0("mod1.",i)))$like)
for(i in 1:8)   lt1 <- cbind(lt1,summary(get(paste0("mod1.",i)))$like)
for(i in 10:14) lt1 <- cbind(lt1,summary(get(paste0("mod1.",i)))$like)
lt1


tmp <- combineModels(mod1.0 ,mod1.7,mod1.11)
tmp <- combineModels(mod1.11 ,mod2.0,mod1.17)
plot(tmp,combine=T,what="recruitment",stack=F)
plot(tmp,combine=T,what="biomass",stack=F)
plot(tmp,combine=T,what="fishing_mortality",stack=F)
mod0_14 <- combineModels(mod0.1 ,mod0.13,mod1.14)
plot(mod0_14,combine=T,what="recruitment",stack=F)

mod0_8 <- combineModels(mod0.0,mod0.1, mod0.2, mod0.3, mod0.4, mod0.5, mod0.6 ,mod0.7,mod0.8)
mod7_8 <- combineModels(mod0.7 ,mod0.8)
mod1415 <- combineModels(mod1.14 ,mod1.15)
plot(mod1415,combine=T,what="recruitment",stack=F)
mod4_5 <- combineModels(mod0.4 ,mod0.5)
mod9_10 <- combineModels(mod0.9 ,mod0.10)
mod10_11 <- combineModels(mod0.10 ,mod0.11)
mod10_11 <- combineModels(mod1.0 ,mod1.11)
mod10_17 <- combineModels(mod1.0 ,mod1.7)
plot(mod10_11,combine=T,what="recruitment",stack=F)
plot(mod10_11,combine=T,what="biomass",stack=F)
1G1Gplot(mod10_17,combine=T,what="recruitment",stack=F)
plot(mod10_17,combine=T,what="biomass",stack=F)

mod1_5 <- combineModels(mod1.0 ,mod1.5)
plot(mod1_5,combine=T,what="recruitment",stack=F)
plot(mod1_5,combine=T,what="biomass",stack=F)

plot(mod10_11,combine=T,what="recruitment",stack=F)
plot(mod10_11,combine=T,what="biomass",stack=F)
plot(mod9_10,combine=T,what="recruitment",stack=F)
plot(mod0_8,combine=T,what="recruitment",stack=F)
plot(mod7_8,combine=T,what="recruitment",stack=F)
plot(mod0_7,combine=T,what="biomass",stack=F)
plot(mod9_10,combine=T,what="biomass",stack=F)
plot(mod6_7,combine=T,what="biomass",stack=F)
plot(mod4_5,combine=T,what="biomass",stack=F)
plot(mod0_7,combine=T,what="fits",stack=F)


# 2015 script d
mod2.3 <- runJJM("mod2.3", path="config", input="input",exec="jjm",version="2014")
mod1.0 <- runJJM("mod1.0", path="config", input="input",exec="jjm",version="2015")
mod1.0 <- readJJM("mod1.0", path="config", input="",exec="jjm",version="2015")
mod2.3 <- readJJM("mod2.3", path="config", input="input",exec="jjm",version="2014")
modxx = combineModels(mod1.0,mod2.3)
plot(modxx,combine=T,what="recruitment",stack=F)
plot(modxx,combine=T,what="biomass",stack=F)
mod1.4 = combineModels(mod1.0,mod0.4)
plot(mod1.4,combine=T,what="recruitment",stack=F)
plot(mod1.4,combine=T,what="biomass",stack=F)
mod1.0d <- diagnostics(mod1.0)
plot(mod1.0d)

  # Two stock runs
#mod0.0ms2 <- runJJM("mod0.0ms2", path="config", input="input",exec="jjms")
mod0.1ms2 <- runJJM("mod0.1ms2", path="config", input="input",exec="jjms")
#mod0.2ms2 <- runJJM("mod0.2ms2", path="config", input="input", version="2015",exec="jjms")
#mod0.3ms2 <- runJJM("mod0.3ms2", path="config", input="input", version="2015",exec="jjms")
#mod0.4ms2 <- runJJM("mod0.4ms2", path="config", input="input", version="2015",exec="jjms")
mod0.0ms2 <- readJJM("mod0.0ms2", path="config", input="input",exec="jjms")
mod0.2ms2 <- readJJM("mod0.2ms2", path="config", input="input", version="2015",exec="jjms")
mod0.3ms2 <- readJJM("mod0.3ms2", path="config", input="input", version="2015",exec="jjms")
mod0.4ms2 <- readJJM("mod0.4ms2", path="config", input="input", version="2015",exec="jjms")


modxx = combineModels(mod0.0)
mod00 = combineModels(mod0.0,mod0.1,mod0.2,mod0.3,mod0.4)
mod02 = combineModels(mod0.0,mod0.2)
mod02 = combineModels(mod0.0,mod0.2)
mod24 = combineModels(mod0.2,mod0.4)
mod034 = combineModels(mod0.3,mod0.4)
mod04 = combineModels(mod0.0,mod0.4)
mod46 <- combineModels(mod0.4,mod0.6)
mod0.2$output
(lstOuts[[2]]$SSB)


# Risk tables
lstOuts <- list(Model_2=mod0.2[[1]]$output[[1]],Model_4=mod0.4[[1]]$output[[1]])
matrix(NA,nrow=length(lstOuts),ncol=length(grep("SSB_fut_",names(lstOuts[[1]]))),
                      dimnames=list(names(lstOuts),1:length(grep("SSB_fut_",names(lstOuts[[1]])))))

# Get the future SSBs and SDs together in one file
fut       <- do.call(rbind,lapply(lstOuts,function(x){
                     do.call(rbind,lapply(x[grep("SSB_fut_",names(x))],
                                          function(y){return(y[,1:3])}))}))
fut       <- as.data.frame(fut,stringsAsFactors=F)
colnames(fut) <- c("year","SSB","SD")
fut$modelscenario <- paste(rep(names(lstOuts),each=nrow(lstOuts[[1]]$SSB_fut_1) *
                                                   length(grep("SSB_fut_",names(lstOuts[[1]])))),
                           paste("Scen",
                                 rep(1:length(grep("SSB_fut_",names(lstOuts[[1]]))),each=nrow(lstOuts[[1]]$SSB_fut_1)),
                                 sep="_"),
                           sep="_")
#Get the 2012 SSB and SDs together
fut
ass       <- do.call(rbind,lapply(lstOuts,function(x){
                                  x$SSB[which(x$SSB[,1] == (x$SSB_fut_1[1,1]-1)),1:3]}))
ass       <- as.data.frame(ass,stringsAsFactors=F)
colnames(ass) <- c("year","SSB","SD")
ass$modelscenario <- names(lstOuts)
ass


#Do the risk calculation:
# Each final year, what is the risk of SSB(final year) < SSB(ass year)
rsktable    <- matrix(NA,nrow=length(lstOuts),ncol=length(grep("SSB_fut_",names(lstOuts[[1]]))),
                      dimnames=list(names(lstOuts),1:length(grep("SSB_fut_",names(lstOuts[[1]])))))
ratiotable  <- matrix(NA,nrow=length(lstOuts),ncol=length(grep("SSB_fut_",names(lstOuts[[1]]))),
                      dimnames=list(names(lstOuts),1:length(grep("SSB_fut_",names(lstOuts[[1]])))))

for(i in names(lstOuts)){
  futdat <-subset(fut,year==max(fut$year) &
                  paste("Model_",unlist(strsplit(fut$modelscenario,"_"))[seq(2,nrow(fut)*4,4)],sep="") == i)
  assdat <- subset(ass,modelscenario == i)
  rsktable[i,] <- dnorm(assdat$SSB,futdat$SSB,futdat$SD)
  ratiotable[i,] <- round((futdat$SSB / assdat$SSB),2 )# / futdat$SSB *100,1)
}
rsktable
ratiotable
plot(mod00,combine=T,what="recruitment",stack=F)
plot(mod00,combine=T,what="biomass",stack=F)
plot(mod02,combine=T,what="recruitment",stack=F)
pdf("results/2v4.pdf")
grid(nx=1,ny=2)

plot(mod24,combine=T,what="recruitment",stack=F)
plot(mod24,combine=T,what="biomass",stack=F)
plot(mod04,combine=T,what="recruitment",stack=F)
plot(mod04,combine=T,what="biomass",stack=F)

plot(mod24, what="projections",var="ssbPrediction",ylab="SSB (kt)",main="Model 0.4", ylim=c(0, 15000), xlim=c(2000, 2025))

legend(2020,15000,1:3)


mod034 = combineModels(mod0.3,mod0.4)
plot(mod46,combine=T,what="recruitment",stack=F)
plot(mod46,combine=T,what="biomass",stack=F)
plot(mod00,combine=T)
plot(mod00,combine=T,what="recruitment",stack=F)
plot(mod02,combine=T,what="biomass",stack=F)
plot(mod03,combine=T,what="recruitment",stack=F)
plot(mod03,combine=T,what="biomass",stack=F)
plot(mod034,combine=T,what="biomass",stack=F)
plot(mod034,combine=T,what="recruitment",stack=F)
plot(mod034,combine=T,what="fits",stack=F)

logLik(mod00,details=T)
summary(mod00)$lik
plot(mod1.0,what="fits")
s1 = summary(mod0.0)$like
s2 = summary(mod0.1)$like
s3 = summary(mod0.2)$like
s4 = summary(mod0.3)$like
s2
merge(s1,s2,s3, by = "row.names")
merge(s2,s3, by = "row.names")
merge(s3,s4, by = "row.names")
# Name models in a list
Mod0List = paste0("mod0.", 0:3)
Mod1List = paste0("mod1.", c(0:5,9,10,11))

# Last year's model....
Mod2014 = "mod1.11"

# Run models --------------------------------------------------------------
#runJJM(modelName = Mod1List, path = reposDir)
mod2014  = readJJM(model  = Mod2014, path = reposDir)
mod1n.11 = readJJM(model  = Mod1Listn[9], path = reposDir)

jjmR:::.kobeFUN2(mod2.0)
kobe(mod0.3)
kobe(mod2.1,add=T)
kobe(mod2.2)
kobe(mod2.3)
mod0.1 = readJJM(model  = Mod0List[2], path = reposDir)
mod0.2 = readJJM(model  = Mod0List[3], path = reposDir)
mod4.1 = readJJM(model  = Mod2013, path = reposDir)

mod1.0 = readJJM(model  = Mod1List[1], path = reposDir)
mod1.1 = readJJM(model  = Mod1List[2], path = reposDir)
mod1.2 = readJJM(model  = Mod1List[3], path = reposDir)
mod1.3 = readJJM(model  = Mod1List[4], path = reposDir)
mod1.4 = readJJM(model  = Mod1List[5], path = reposDir)
mod1.5 = readJJM(model  = Mod1List[6], path = reposDir)
mod1.6 = readJJM(model  = Mod1List[7], path = reposDir)
mod1.7 = readJJM(model  = Mod1List[8], path = reposDir)
mod1.9 = readJJM(model  = Mod1List[7], path = reposDir)
mod1.10= readJJM(model  = Mod1List[8], path = reposDir)
mod1.11= readJJM(model  = Mod1List[9], path = reposDir)
mod2.0 = readJJM(model  = "mod2.0",    path = reposDir)
mod2.1 = readJJM(model  = "mod2.1",    path = reposDir)
mod2.2 = readJJM(model  = "mod2.2",    path = reposDir)
mod2.3 = readJJM(model  = "mod2.3",    path = reposDir)
with(mod2.3$output$output,msy_mt)
	{ cbind( Catch_fut_1[,1:2], Catch_fut_2[,2], Catch_fut_3[,2], Catch_fut_4[,2])})
names(mod2.0$output$output)
cfut0 = with(mod2.0$output$output,{ cbind( Catch_fut_1[,1:2], Catch_fut_2[,2], Catch_fut_3[,2], Catch_fut_4[,2])})
cfut2 = with(mod2.2$output$output,{ cbind( Catch_fut_1[,1:2], Catch_fut_2[,2], Catch_fut_3[,2], Catch_fut_4[,2])})
cfut3 = with(mod2.3$output$output,{ cbind( Catch_fut_1[,1:2], Catch_fut_2[,2], Catch_fut_3[,2], Catch_fut_4[,2])})
cfut3

cfut = with(mod2.1$output$output,{ cbind( Catch_fut_1[,1:2], Catch_fut_2[,2], Catch_fut_3[,2], Catch_fut_4[,2])})
cfut = with(mod2.2$output$output,{ cbind( Catch_fut_1[,1:2], Catch_fut_2[,2], Catch_fut_3[,2], Catch_fut_4[,2])})
with(mod2.3$output$output,{ cbind( Catch_fut_1[,1:2], Catch_fut_2[,2], Catch_fut_3[,2], Catch_fut_4[,2])})
ssbfut0= with(mod2.0$output$output,{ cbind(  SSB_fut_1[,1:2],  SSB_fut_2[,2],  SSB_fut_3[,2],  SSB_fut_4[,2],SSB_fut_5[,2])})
ssbfut2= with(mod2.2$output$output,{ cbind(  SSB_fut_1[,1:2],  SSB_fut_2[,2],  SSB_fut_3[,2],  SSB_fut_4[,2],SSB_fut_5[,2])})
ssbfut= with(mod2.1$output$output,{ cbind(  SSB_fut_1[,1:2],  SSB_fut_2[,2],  SSB_fut_3[,2],  SSB_fut_4[,2],SSB_fut_5[,2])})
ssbfut= with(mod2.2$output$output,{ cbind(  SSB_fut_1[,1:2],  SSB_fut_2[,2],  SSB_fut_3[,2],  SSB_fut_4[,2],SSB_fut_5[,2])})
with(mod2.3$output$output,{ cbind(  SSB_fut_1[,1:2],  SSB_fut_2[,2],  SSB_fut_3[,2],  SSB_fut_4[,2],SSB_fut_5[,2])})
cfut0

par(mfrow=c(1,1))
plot(cfut[,2],ssbfut[,2],ylim=c(0,8000),xlim=c(0,1500),ylab="SSB (kt)",xlab="Catch")
text(cfut[,2],ssbfut[,2],cfut[,1])
text(cfut2[,2],ssbfut2[,2],cfut[,1],col="red")
text(cfut2[,3],ssbfut2[,3],cfut[,1],col="red")
text(cfut2[,4],ssbfut2[,4],cfut[,1],col="red")
text(cfut2[,5],ssbfut2[,5],cfut[,1],col="red")
text(cfut2[,6],ssbfut2[,6],cfut[,1],col="red")
text(cfut0[,2],ssbfut0[,2],cfut[,1],col="red")

par(mfrow=c(2,1))
plot(ssbfut[,1],ssbfut[,2],ylim=c(0,8000),ylab="SSB (kt)",xlab="Year")
lines(ssbfut[,1],ssbfut[,3])
lines(ssbfut[,1],ssbfut[,4])
lines(ssbfut[,1],ssbfut[,5])
lines(ssbfut[,1],ssbfut[,6])
plot(cfut[,1],cfut[,2],ylim=c(0,1000),ylab="Catch (kt)",xlab="Year")
lines(cfut[,1],cfut[,3])
lines(cfut[,1],cfut[,4])
lines(cfut[,1],cfut[,5])

,ylim=c(0,1000),ylab="Catch (kt)",xlab="Year")
with(mod2.0$output$output,{ cbind(  SSB_fut_1[,1:2],  SSB_fut_2[,2],  SSB_fut_3[,2],  SSB_fut_4[,2],SSB_fut_5[,2])})

mod2.1$output$output$Catch_fut_1
mod2.1$output$output$Catch_fut_1
mod2.1$output$output$Catch_fut_1

mod1234 = combineModels(mod1.0,mod1.1, mod1.2, mod1.3, mod1.4,mod1.5,mod1.9,mod1.10,mod1.11)
mod01 = combineModels(mod1.0,mod1.1)
mod02 = combineModels(mod1.0,mod1.2)
mod03 = combineModels(mod1.0,mod1.3)
mod04 = combineModels(mod1.0,mod1.4)
mod05 = combineModels(mod1.0,mod1.5)
mod09 = combineModels(mod1.0,mod1.9)
mod010= combineModels(mod1.0,mod1.10)
mod011= combineModels(mod1.0,mod1.11)
mod02013 = combineModels(mod1.0,mod4.1)
mod002013 = combineModels(mod0.0,mod4.1)
mod2.3.2013 = combineModels(mod2.3,mod4.1)
mod2.0.2013 = combineModels(mod2.0,mod4.1)
mod0001 = combineModels(mod0.0,mod0.1)
mod0002 = combineModels(mod0.0,mod0.2)
plot(mod2.0.2013)
plot(mod0001)
plot(mod0002)
logLik(mod1234,details=T)
summary(mod1234)$lik
plot(mod1234)
plot(mod02013)
plot(mod002013)
plot(mod01)
plot(mod02)
plot(mod03)
plot(mod05)
plot(mod09)
plot(mod010)
plot(mod011)
names(mod1234$combined$outputs)
,file=file.path(reposDir,"likTable.csv")
logLik(mod1234)
#Survey lik
mods  <- mod1234
surfLik   <- do.call(cbind,lapply(mods$combined$outputs,function(x){do.call(rbind,x[grep("Survey_Index_",names(x))])}))
colnames(surfLik) <- mods$info

#Age survey lik
mods  <- mod123
ageSurvLik   <- do.call(cbind,lapply(mods$combined$outputs,function(x){do.call(rbind,x[grep("Age_Survey_",names(x))])}))
colnames(ageSurvLik) <- mods$info

rbind(surfLik,ageSurvLik)

Print_Figs <- function(Mod,outname="Modx.pdf"){
	pdf(outname,height=9,width=7)
  diagPlots = diagnostics(outputObject = Mod)
  plot(diagPlots, what = "input")
  plot(diagPlots, what = "fit")
  plot(diagPlots, what = "projections")
  dev.off()
}
Print_Figs(mod1n.11,"../figs/Mod1.11n.pdf")
Print_Figs(mod1.0,"../figs/Mod1.0.pdf")
Print_Figs(mod1.4,"../figs/Mod1.4.pdf")
Print_Figs(mod1.2,"../figs/Mod1.2.pdf")
Print_Figs(mod1.3,"../figs/Mod1.3.pdf")
Print_Figs(mod1.5,"../figs/Mod1.5.pdf")
Print_Figs(mod1.10,"../figs/Mod1.10.pdf")

Print_Figs(mod2.2,"../figs/Mod2.2.pdf")
Print_Figs(mod2.3,"../figs/Mod2.3.pdf")

# Reading -----------------------------------------------------------------
# OUTPUT Object
model = readJJM(model= modelName, path = reposDir)

# DIAG object
diagPlots = diagnostics(outputObject = model)

# Combine models ----------------------------------------------------------
plot(mod1234)
plot(mod1234,xlim=c(2000,2015))
plot(mod1234,xlim=c(2010, 2015), ylim=c(2,8), legendPos = "topleft")
mod012 = combineModels(mod0,mod1, mod2kod3)
mod012 = combineModels(mod0,mod1, mod2kod3)
plot(mod012)

# Integrating models ------------------------------------------------------
mod12 = combineStocks(mod1, mod2, model = "mod2s_12")

# Print -------------------------------------------------------------------

# Output object
print(model)

# List of outputs 
print(mod1234)

# Diagnostics object
print(diagPlots)

# Get and print summaries -------------------------------------------------
# Output object
sumModel = summary(model)
sumModel = summary(mod0.0)
print(sumModel)

# List of outputs object
sumList = summary(mod012)
print(sumList)
# Diagnostics object
sumPlots = summary(diagPlots)
sumPlots

mod="mod2.0"


# Get and print plots -----------------------------------------------------
plot(diagPlots, what = "input")
plot(diagPlots, what = "fit")
pdf()
plot(dp, what = "fit")
plot(diagPlots, what = "projections")
plot(diagPlots, what = "ypr")

mod = "mod1.6_2stk"
model <- readJJM(mod, path="config", input="input")
k1=jjmR:::.kobeFUN(model[[1]]$output$Stock_1, xlim=c(0,5), ylim=c(0,8))
plot(k1)
jjmR:::.kobeFUN(model[[1]]$output$Stock_2, pic.add = k1, add = T, xlim=c(0,5), ylim=c(0,8))
