# ------------------------------------------------------------------------
# Script used for SC03------------------------------------------------------------
# ------------------------------------------------------------------------
#install.packages("devtools")
#library(devtools)
#devtools::install_github("sprfmo/jjm", subdir = "/jjmR", ref = "2015")
library(jjmR)

# Set parameters ----------------------------------------------------------
# Path of JJM repository (from current working directory)
setwd("/Users/jim/_mymods/sprfmo/jjm/assessment")
runJJM("mod2014_h65", path="config", input="input", exec="jjm")
mod2014_h65 <- runit("mod2014_h65",portrait=F)

# single stock runs
mod2.3 <- runJJM("mod2.3", path="config", input="input",exec="jjm",version="2014")
mod1.0 <- runJJM("mod1.0", path="config", input="input",exec="jjm",version="2014")
mod1.0 <- readJJM("mod1.0", path="config", input="input",exec="jjm",version="2014")
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


# Model with same avg weight at age as in SC Chile for offshore in 2014 and 2015
mod0.6 <- runit("mod0.6",portrait=F,est=T)

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
plot(mod0.0,what="fits")
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

kobe(mod2014)
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

# Silly shortcut
runit <- function(mod="",est=F,portrait=T,exec="jjm",version="2014",pdf=F){
	if (est)
    runJJM(mod, path="config", input="input", version=version,exec=exec)
  modtmp <- readJJM(mod, path="config", input="input", version="2014")
  if(pdf){
		if (portrait)
    	pdf(paste0("results/",mod,".pdf"),height=9,width=7)
  	else
    	pdf(paste0("results/",mod,".pdf"),height=7,width=9)
  	plot(diagnostics(modtmp))
  	dev.off()
  	}
  return(modtmp)
}

# Get and print plots -----------------------------------------------------
plot(diagPlots, what = "input")
plot(diagPlots, what = "fit")
pdf()
plot(dp, what = "fit")
plot(diagPlots, what = "projections")
plot(diagPlots, what = "ypr")


# Run models in parallel---------------------------------------------------
Mod0List = paste0("mod0.", 0:2)
nCores = 6
cl = makeCluster(rep("localhost",times=nCores), type = "SOCK")
registerDoSNOW(cl)
runJJM(models = Mod0List, path = reposDir, parallel=TRUE)
stopCluster(cl)