# ==================================================================================================================
# === Source code for converting Jim's Jack Mackerel MLE and MCMC output into an OpenMSE operating model ===========
# ==================================================================================================================

# Tom Carruthers (tom@bluematterscience.com)
# March 2024


# === Single fleet aggregated OM functions ==========================================================

make_age_array = function(mcmc, type = "N_stock"){
  
  sub = mcmc[mcmc$mcdraw==type,]
  simnos = unique(sub$row.names)
  nsim = length(simnos)
  nage = max(as.numeric(sub$Age))
  yrs = unique(mcmc$Year)
  yrs = min(yrs):max(yrs)
  nyears = length(yrs)
  arr = array(0,c(nsim, nage+1,nyears)) 
  ind = cbind(match(sub$row.names,simnos), as.numeric(sub$Age)+1, match(sub$Year,yrs))
  arr[ind] = sub$value
  arr
  
}

make_age_array2 = function(mcmc, type = "N_stock"){
  
  sub = mcmc[mcmc$mcdraw==type,]
  simnos = unique(mcmc$row.names)
  nsim = length(simnos)
  nage = max(as.numeric(mcmc$Age))
  yrs = min(yrs):max(yrs)
  nyears = length(yrs)
  arr = array(0,c(nsim, nage,nyears)) 
  ind = cbind(match(mcmc$row.names,simnos), as.numeric(mcmc$Age), match(mcmc$Year,yrs))
  arr[ind] = mcmc$value
  arr
  
}


docomp = function(yrs,est,sim,lab=""){
  plot(yrs,est,col="#0000ff95",pch=19,xlab="",ylab="")
  lines(yrs,sim,col="red"); 
  mtext(lab,cex=0.8,line=0.2)
}

plotJMcomp=function(mod,naa,faa,Hist,totcatch,simnos = 1:2,nfleet = 4){
  
  est0 = mod[[1]]$output$Stock_1
  F_est = est0$TotF
  N_est = est0$N
  B_est = apply(est0$TotBiom[,2:5],1,sum)
  cols = 2:ncol(est0$C_fsh_1)
  C_est = apply(est0$C_fsh_1[,cols]*est0$wt_fsh_1[,cols],1,sum)
  for(ff in 2:nfleet){
    C_est = C_est + apply(est0[[paste0("C_fsh_",ff)]][,cols]*est0[[paste0("wt_fsh_",ff)]][,cols],1,sum)
  }
  F_sim = Hist@AtAge$F.Mortality[,,,1]            # mixed model so area 1 is indicative
  N_sim = apply(Hist@AtAge$Number,1:3,sum)        # sum over areas
  B_sim = apply(Hist@TSdata$Biomass,1:2,sum)
  C_sim = apply(Hist@TSdata$Landings,1:2,sum)
  yrs = Hist@Data@Year
  
  par(mfrow=c(5,2),mai=c(0.3,0.4,0.3,0.01))
  # F 
  age = 3; 
  docomp(yrs,est=faa[simnos[1],age+1,],sim = F_sim[simnos[1],age+1,],paste("Age",age,"F, sim ",simnos[1]))
  legend('topleft',legend=c("JMest","openMSE"),text.col=c("blue",'red'),bty="n",cex=0.8)
  docomp(yrs,est=faa[simnos[2],age+1,],sim = F_sim[simnos[2],age+1,],paste("Age",age,"F, sim ",simnos[2]))
  
  age = 8; 
  docomp(yrs,est=faa[simnos[1],age+1,],sim = F_sim[simnos[1],age+1,],paste("Age",age,"F, sim ",simnos[1]))
  docomp(yrs,est=faa[simnos[2],age+1,],sim = F_sim[simnos[2],age+1,],paste("Age",age,"F, sim ",simnos[2]))
  
  
  # N 
  age = 1
  docomp(yrs,est=naa[simnos[1],age+1,],sim = N_sim[simnos[1],age+1,],paste("Age",age,"Numbers, sim ",simnos[1]))
  docomp(yrs,est=naa[simnos[2],age+1,],sim = N_sim[simnos[2],age+1,],paste("Age",age,"Numbers, sim ",simnos[2]))

  age = 5
  docomp(yrs,est=naa[simnos[1],age+1,],sim = N_sim[simnos[1],age+1,],paste("Age",age,"Numbers, sim ",simnos[1]))
  docomp(yrs,est=naa[simnos[2],age+1,],sim = N_sim[simnos[2],age+1,],paste("Age",age,"Numbers, sim ",simnos[2]))
  
  docomp(yrs,est = C_est, sim=C_sim[simnos[1],], "Total landings")
  legend('topleft',legend=c("JMest (MLE fit)","openMSE"),text.col=c("blue",'red'),bty="n",cex=0.8)
  docomp(yrs,est = C_est, sim=C_sim[simnos[2],], "Total landings")
  
  
}


# proyears = 50; interval = 2; nsim=48; seed = 1; check=T; datastart = 2012
JM2OM = function(mod, mceval_file, proyears = 50, interval = 2, nsim=48, seed = 1, check=T, datastart = NA){
  
  info = mod[[1]]$info        #1
  data = mod[[1]]$data        #2
  cn = mod[[1]]$control       #3
  pars = mod[[1]]$parameters  #4
  MLE = mod[[1]]$output[[1]]  #5
  
  # lapply(mod,FUN = function(x)lapply(x,names)) # check list structure / names
 
  yrng = data$years
  yrs = yrng[1]:yrng[2]
  nyears = length(yrs)
  nage = info$data$age[2]
  
  cat("Loading MCMC file \n")
  mcmc0 = read.csv(mceval_file,row.names = NULL)
  cat("MCMC file loaded \n")
  
  nits = max(as.numeric(mcmc0$row.names))
  
  set.seed(seed); cat("Setting random seed \n")
  cat("Sampling from MCMC fishing mortatlity rates and numbers at age \n")
  mcmc = mcmc0[mcmc0$row.names %in% sample(1:nits,nsim) & mcmc0$Year !="all", ]  # work with a much smaller object
  nsim = length(unique(mcmc$row.names))
  
  # check dimensions
  mcmc_lastyr = as.numeric(max(mcmc$Year))
  if(mcmc_lastyr != yrng[2]){
    cat(paste0("Model files indicate final year is ",yrng[2]," \n"))
    cat(paste0("MCMC output provided to ",mcmc_lastyr," \n"))
    cat("Using MCMC final year for Assess2OM() \n")
    yrng[2] = mcmc_lastyr
    yrs = yrng[1]:yrng[2]
    nyears = length(yrs)
  }
  
  # arrays for Assess2OM()
  naa = make_age_array(mcmc,type= "N_stock")
  naa[,1,1:(nyears-1)] = naa[,2,2:nyears] * exp(rep(MLE$M[1:nyears,1],each=nsim)) ; cat("Age 0 numbers are imputed based on age 1 numbers and natural mortality rate \n")
  
  faa = make_age_array(mcmc, "F_stock"); cat("Fishing mortality rate is assumed to be zero for age 0 fish \n")
  faa[faa==0] = 1E-3
  waa = array(rep(c(0,MLE$wt_a_pop),each=nsim),c(nsim,nage+1,nyears)); cat("wt_a_pop used. Age zero fish are assumed to have weight zero \n") 
  Mataa = array(rep(c(0,MLE$mature_a),each=nsim),c(nsim,nage+1,nyears)); cat("mature_a used. Maturity = 0 is assumed for age zero fish \n") 
  Maa = abind(matrix(rep(MLE$M[1:nyears,1],each=nsim),nrow=nsim),array(rep(t(MLE$M[1:nyears,]),each=nsim),c(nsim,nage,nyears)),along=2); cat("Age zero fish assumed to have same natural mortality rate as age 1 fish \n")
  len_a = cn$Lo_len[1,1] + cn$Linf[1,1] * (1-exp(-(0:nage)*cn$K[1,1]))
  laa = array(rep(len_a,each=nsim), c(nsim,nage+1,nyears)); cat("Somatic growth assumed to be Lo_len + Linf * (1-exp(-ak)) \n")
  
 
  OM = Assess2OM(Name = cn$modelName, proyears, interval, yrng[2], h = pars$steepness, recind=0,
                 naa = naa, faa = faa, waa = waa, Mataa = Mataa, Maa = Maa, laa = laa)
  
  OM@LenCV = cn$Sigma_len[1,1]
  OM@Misc$Inames = data$Inames
  OM@Misc$fleet_names = data$fleet_names
  
  # now add real data (to simulate statistical properties in projections)
  Data = new('Data')
  nI = length(data$Inames)
  cat(paste0("Indices are: ", paste(paste0("(",1:nI,")"),data$Inames,collapse=", "), "\n"))
  if(nrow(data$Index)>nyears) cat(paste0("The model currently ends (",yrng[2],") before the specified index data series ends (",max(rownames(data$Index)),") \n"))
  Data@AddInd = array(rep(t(data$Index[1:nyears,]),each=nsim),c(nsim,nI,nyears))
  
  Data@CV_AddInd = array(rep(t(data$Indexerr[1:nyears,]/data$Index[1:nyears,]),each=nsim),c(nsim,nI,nyears))
  Data@AddIndV = array(0, c(nsim,nI,nage+1))
  for(i in 1:nI) Data@AddIndV[,i,2:(nage+1)] = rep(MLE[[paste0("sel_ind_",i)]][nyears,2+(1:nage)],each=nsim); cat("Index vulnerabilities assumed to be those most recent estimated \n")
  
  if(!is.na(datastart))   Data@AddInd[,,yrs<datastart] = NA;  cat(paste0("Removing observations before ", datastart, " for calculation of observation error properties \n"))

  OM@cpars$Data = Data
  
  if(check){
    cat("Checking conditioning model N and F against OpenMSE reconstructed N and F \n")
    Hist = runMSE(OM,Hist=T)
    plotJMcomp(mod,naa,faa,Hist)
  }
  
  OM
  
}


# === Multi operating model (multiple stocks / fleets) MOM functions =================================================

# stockno = 1; fleetno =1; simnos = 1:2; nfleet = 4
plotJMcomp_MOM = function(mod,naa,faa,waa,Hist,stockno = 1, fleetno = 1, simnos = 1:2, nfleet = 4){
  
  est0 = mod[[1]]$output$Stock_1
  F_est = est0$TotF
  N_est = est0$N
  B_est = apply(waa*naa,c(1,3),sum)
  nage = dim(naa)[2]
  waa_ya = t(waa[1,2:nage,,stockno])
  cols = 2:ncol(est0$C_fsh_1)
  C_est = apply(est0$C_fsh_1[,cols]*waa_ya, 1,sum) #est0$wt_fsh_1[,cols],1,sum)
  for(ff in 2:nfleet){
    C_est = C_est + apply(est0[[paste0("C_fsh_",ff)]][,cols]*waa_ya, 1, sum) #est0[[paste0("wt_fsh_",ff)]][,cols],1,sum)
  }  
  sHist = Hist[[stockno]][[fleetno]]
  F_sim = sHist@AtAge$F.Mortality[,,,1]            # mixed model so area 1 is indicative
  N_sim = apply(sHist@AtAge$Number,1:3,sum)        # sum over areas
  B_sim = apply(sHist@TSdata$Biomass,1:2,sum)
  yrs = sHist@Data@Year
  C_sim = apply(Hist[[1]][[1]]@TSdata$Landings,1:2,sum)
  for(ff in 2:nfleet)C_sim = C_sim + apply(Hist[[1]][[ff]]@TSdata$Landings,1:2,sum)
  
  par(mfrow=c(6,2),mai=c(0.25,0.4,0.25,0.01))
  
  # F 
  age = 3; 
  docomp(yrs, est = faa[simnos[1],age+1,,stockno,fleetno],sim = F_sim[simnos[1],age+1,],paste("Age",age,"F, sim ",simnos[1],"Fleet",fleetno))
  legend('topleft',legend=c("JMest","openMSE"),text.col=c("blue",'red'),bty="n",cex=0.8)
  docomp(yrs,est=faa[simnos[2],age+1,,stockno,fleetno],sim = F_sim[simnos[2],age+1,],paste("Age",age,"F, sim ",simnos[2],"Fleet",fleetno))
  
  age = 8; 
  docomp(yrs,est=faa[simnos[1],age+1,,stockno,fleetno],sim = F_sim[simnos[1],age+1,],paste("Age",age,"F, sim ",simnos[1],"Fleet",fleetno))
  legend('topleft',legend=c("JMest","openMSE"),text.col=c("blue",'red'),bty="n",cex=0.8)
  docomp(yrs,est=faa[simnos[2],age+1,,stockno,fleetno],sim = F_sim[simnos[2],age+1,],paste("Age",age,"F, sim ",simnos[2],"Fleet",fleetno))
  
  # N 
  age = 1
  docomp(yrs,est=naa[simnos[1],age+1,,stockno],sim = N_sim[simnos[1],age+1,],paste("Age",age,"Numbers, sim ",simnos[1]))
  legend('topleft',legend=c("JMest","openMSE"),text.col=c("blue",'red'),bty="n",cex=0.8)
  docomp(yrs,est=naa[simnos[2],age+1,,stockno],sim = N_sim[simnos[2],age+1,],paste("Age",age,"Numbers, sim ",simnos[2]))
  
  age = 5
  docomp(yrs,est=naa[simnos[1],age+1,,stockno],sim = N_sim[simnos[1],age+1,],paste("Age",age,"Numbers, sim ",simnos[1]))
  legend('topleft',legend=c("JMest","openMSE"),text.col=c("blue",'red'),bty="n",cex=0.8)
  docomp(yrs,est=naa[simnos[2],age+1,,stockno],sim = N_sim[simnos[2],age+1,],paste("Age",age,"Numbers, sim ",simnos[2]))
  
  # Total biomass and landings
  docomp(yrs,est = B_est[simnos[1],], sim=B_sim[simnos[1],], paste("Biomass sim",simnos[[1]]))
  legend('topleft',legend=c("JMest","openMSE"),text.col=c("blue",'red'),bty="n",cex=0.8)
  docomp(yrs,est = B_est[simnos[2],], sim=B_sim[simnos[2],], paste("Biomass sim",simnos[[2]]))
  
  docomp(yrs,est = C_est, sim=C_sim[simnos[1],], paste("Total landings sim",simnos[[1]]))
  legend('topleft',legend=c("JMest (MLE fit)","openMSE"),text.col=c("blue",'red'),bty="n",cex=0.8)
  docomp(yrs,est = C_est, sim=C_sim[simnos[2],], paste("Total landings sim",simnos[[2]]))
  
  
}


make_N_array = function(mcmc, nstock = 1){
  
  sub = mcmc[mcmc$type=="N_stock",]
  simnos = unique(sub$mcdraw)
  nsim = length(simnos)
  nage = max(as.numeric(sub$Age))
  yrs = unique(mcmc$Year)
  yrs = min(yrs):max(yrs)
  nyears = length(yrs)
  arr = array(NA,c(nsim, nage+1,nyears,nstock)) 
  
  ind = cbind(match(sub$mcdraw,simnos), as.numeric(sub$Age)+1, match(sub$Year,yrs),rep(1,nrow(sub)))
  arr[ind] = sub$value
  arr
  
}

make_F_array = function(mcmc, nstock = 1, nfleet = 4){
  
  sub = mcmc[mcmc$type=="F_stock",]
  simnos = unique(sub$mcdraw)
  nsim = length(simnos)
  nage = max(as.numeric(sub$Age))
  yrs = unique(mcmc$Year)
  yrs = min(yrs):max(yrs)
  nyears = length(yrs)
  arr = array(0,c(nsim, nage+1,nyears,nstock,nfleet)) 
  
  ind = cbind(match(sub$mcdraw,simnos), as.numeric(sub$Age)+1, match(sub$Year,yrs),rep(1,nrow(sub)),as.numeric(sub$fleet))
  arr[ind] = sub$value
  arr
  
}

# proyears = 50; interval = 2; nsim=48; seed = 1; check=T; datastart = 2012; LowerTri = 1; nstock = 1; nfleet = 4
JM2MOM = function(mod, mceval_file, proyears = 50, interval = 2, nsim=48, seed = 1, check=T, datastart = NA, LowerTri=1, 
                  nstock = 1, nfleet = 4){
  
  info = mod[[1]]$info        #1
  data = mod[[1]]$data        #2
  cn = mod[[1]]$control       #3
  pars = mod[[1]]$parameters  #4
  MLE = mod[[1]]$output[[1]]  #5
  
  # lapply(mod,FUN = function(x)lapply(x,names)) # check list structure / names
  
  yrng = data$years
  yrs = yrng[1]:yrng[2]
  nyears = length(yrs)
  nage = info$data$age[2]
  
  cat("Loading MCMC file \n")
  mcmc0 = read.csv(mceval_file,row.names = NULL)
  cat("MCMC file loaded \n")
  
  nits = max(as.numeric(mcmc0$mcdraw))
  
  set.seed(seed); cat("Setting random seed \n")
  cat("Sampling from MCMC fishing mortatlity rates and numbers at age \n")
  mcmc = mcmc0[mcmc0$mcdraw %in% sample(1:nits,nsim) & mcmc0$Year !="all", ]  # work with a much smaller object
   
  # check dimensions
  mcmc_lastyr = as.numeric(max(mcmc$Year))
  if(mcmc_lastyr != yrng[2]){
    cat(paste0("Model files indicate final year is ",yrng[2]," \n"))
    cat(paste0("MCMC output provided to ",mcmc_lastyr," \n"))
    cat("Using MCMC final year for Assess2OM() \n")
    yrng[2] = mcmc_lastyr
    yrs = yrng[1]:yrng[2]
    nyears = length(yrs)
  }
  
  # arrays for Assess2OM()
  naa = make_N_array(mcmc)
  naa[,1,1:(nyears-1),1] = naa[,2,2:nyears,1] * exp(rep(MLE$M[2:nyears,1],each=nsim)) ; cat("Age 0 numbers are imputed based on age 1 numbers and natural mortality rate \n")
  
  faa = make_F_array(mcmc); cat("Fishing mortality rate is assumed to be zero for age 0 fish \n")
  faa[faa==0] = 1E-3
  waa = array(rep(c(0,MLE$wt_a_pop),each=nsim),c(nsim,nage+1,nyears,nstock)); cat("wt_a_pop used. Age zero fish are assumed to have weight zero \n") 
  Mataa = array(rep(c(0,MLE$mature_a),each=nsim),c(nsim,nage+1,nyears,nstock)); cat("mature_a used. Maturity = 0 is assumed for age zero fish \n") 
  Maa_simage = abind(matrix(rep(MLE$M[1:nyears,1],each=nsim),nrow=nsim),array(rep(t(MLE$M[1:nyears,]),each=nsim),c(nsim,nage,nyears)),along=2)
  Maa = array(Maa_simage,c(nsim,nage+1,nyears,nstock)); cat("Age zero fish assumed to have same natural mortality rate as age 1 fish \n")
  len_a = cn$Lo_len[1,1] + cn$Linf[1,1] * (1-exp(-(0:nage)*cn$K[1,1]))
  laa = array(rep(len_a,each=nsim), c(nsim,nage+1,nyears,nstock)); cat("Somatic growth assumed to be Lo_len + Linf * (1-exp(-ak)) \n")
  
  
  OM = Assess2MOM(Name = cn$modelName, proyears, interval, yrng[2], h = pars$steepness, recind=0,
                 naa = naa, faa = faa, waa = waa, Mataa = Mataa, Maa = Maa, laa = laa, 
                 LowerTri = LowerTri)
  
  OM@Stocks[[1]]@LenCV = rep(cn$Sigma_len[1,1],2)
  for(ff in 1:nfleet){
    OM@Fleets[[1]][[ff]]@Misc$Inames = data$Inames
    OM@Fleets[[1]][[ff]]@Misc$fleet_names =  data$fleet_names
  }
  
  # now add real data (to simulate statistical properties in projections)
  Data = new('Data')
  nI = length(data$Inames)
  cat(paste0("Indices are: ", paste(paste0("(",1:nI,")"),data$Inames,collapse=", "), "\n"))
  cat("Adding historical index observations for calculation of statistical properties under projection \n")
  if(nrow(data$Index)>nyears) cat(paste0("The model currently ends (",yrng[2],") before the specified index data series ends (",max(rownames(data$Index)),") \n"))
  Data@AddInd = array(rep(t(data$Index[1:nyears,]),each=nsim),c(nsim,nI,nyears))
  
  Data@CV_AddInd = array(rep(t(data$Indexerr[1:nyears,]/data$Index[1:nyears,]),each=nsim),c(nsim,nI,nyears))
  Data@AddIndV = array(0, c(nsim,nI,nage+1))
  for(i in 1:nI) Data@AddIndV[,i,2:(nage+1)] = rep(MLE[[paste0("sel_ind_",i)]][nyears,2+(1:nage)],each=nsim); cat("Index vulnerabilities assumed to be those most recent estimated \n")
  
  cat("Adding historical catch observations for calculation of statistical properties under projection \n")
  Cat = MLE$Obs_catch_1
  for(ff in 2:nfleet)Cat = Cat + MLE[[paste0("Obs_catch_",ff)]]
  Data@Cat = array(rep(Cat,each=nsim),c(nsim,nyears))
  Data@CV_Cat = array(0.025,c(nsim,nyears))
  
  if(!is.na(datastart)){
    Data@AddInd[,,yrs<datastart] = NA;  cat(paste0("Removing observations before ", datastart, " for calculation of observation error properties \n"))
    #Data@Cat[,yrs<datastart] = NA
  }  
  
  #      stock  fleet
  for(ff in 1:nfleet){
    OM@cpars[[1]][[ff]]$Data = Data # may be able to only put the data object in first stock / fleet as it includes all 5 indices and their vulnerabilities
    OM@cpars[[1]][[ff]]$AddIbeta = array(1,c(nsim,nI))
  }
  
  if(check){
    cat("Checking conditioning model N and F against OpenMSE reconstructed N and F \n")
    Hist = multiMSE(OM,Hist=T)
    plotJMcomp_MOM(mod,naa,faa,waa,Hist)
  }
  
  OM
  
}



# === Multi MSE plots ===========================================================================================


plotquant2<-function(x,p=c(0.05,0.25,0.75,0.95),yrs,qcol,lcol,addline=T,ablines=NA){
  ny<-length(yrs)
  qs<-apply(x,2,quantile,p=p[c(1,4)],na.rm=T)
  qsi<-apply(x,2,quantile,p=p[2:3],na.rm=T)
  if (all(qs[1,]==qs[2,])) {
    lines(yrs, qs[1,], lwd=2, col=qcol)
  } else {
    polygon(c(yrs,yrs[ny:1]),c(qs[1,],qs[2,ny:1]),border=NA,col='#b3ecff')
    polygon(c(yrs,yrs[ny:1]),c(qsi[1,],qsi[2,ny:1]),border=NA,col=qcol)
    lines(yrs,apply(x,2,quantile,p=0.5,na.rm=T),lwd=2,col="white")
  }
  
  if(!is.na(ablines[1]))abline(h=ablines,col='darkgrey')
  
  if(addline)for(i in 1:2)lines(yrs,x[i,],col=lcol,lty=i)
  
}



# maxcol = 6; qcol = rgb(0.4, 0.8, 0.95); lcol = "dodgerblue4"; quants = c(0.05, 0.25, 0.75, 0.95); curyr = 2018; addline = TRUE
plot_proj_JJM <- function(MMSE, maxcol = 6, qcol = rgb(0.4, 0.8, 0.95), lcol = "dodgerblue4",
                      quants = c(0.05, 0.25, 0.75, 0.95), curyr = 2018, addline = TRUE,
                      doplot = 1:4, ...) {

  if (!methods::is(MMSE, 'MMSE')) stop('Object must be class `MMSE`')
  if(is.na(maxcol))maxcol=ceiling(length(MMSE@MPs)/0.5) # defaults to portrait 1:2
  MPs<-MMSE@MPs
  MPrefs<-MMSE@MPrefs
  nMPs<-length(MPrefs[,1,1])
  yrs<-curyr+(1:MMSE@proyears)
  ns<-MMSE@nstocks
  nf<-MMSE@nfleets
  
  plots<-split(1:nMPs, ceiling(seq_along(1:nMPs)/maxcol))
  
  dopar = function()par(mfcol=c(ns,nt),mai=c(0.3,0.3,0.3,0.1),omi=c(0.4,0.25,0.05,0.05))
  xline = 1; yline = 0.3
  ss=1 # just stock level 1
  fMPno = 1 # same MP for every fleet
  
  if(1 %in% doplot){
    # --- Biomass projection ---------------------------------------------------
    B_BMSY<-MMSE@SB_SBMSY
    Blims <- c(0,quantile(B_BMSY,0.95))
   
    for(pp in 1:length(plots)){
      
      toplot<-plots[[pp]]
      nt<-length(toplot)
      dopar()
      
      for(MP in toplot){
        
        plot(range(yrs),Blims,col="white",yaxs="i")
       
        plotquant2(B_BMSY[,ss,MP,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1),addline=addline)
        grid()
        mtext(paste(MPrefs[MP,fMPno,ss],collapse=", "),line=0.2,font=2,cex=0.7)
        
        #if(MP==toplot[1])mtext(MMSE@Snames[ss],2,line=2.5)
      
      }
    }
    
    mtext("Projection Year",1,font=2,outer=T,line=xline)
    mtext("Spawning Biomass relative to MSY levels",2,font=2,outer=T,line=yline)
  }
  
  # --- F projection -----------------------------------------------------------
  
  if(2 %in% doplot){
    FM<-MMSE@FM
    FMsum<-apply(FM,c(1,4,5),sum,na.rm=T)
    Flims<- c(0,quantile(FMsum,0.95,na.rm=T))
    
    for(pp in 1:length(plots)){
      
      toplot<-plots[[pp]]
      nt<-length(toplot)
      dopar()
      
      for(MP in toplot){
        
        plot(range(yrs),Flims,col="white",yaxs="i")
        plotquant2(FMsum[,MP,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1),addline=addline)
        grid()
        mtext(MPrefs[MP,,1],3,line=0.2,font=2,cex=0.7)
       
      }
    }
    
    mtext("Projection Year",1,font=2,outer=T,line=xline)
    mtext("Total apical F (all fleets)",2,font=2,outer=T,line=yline)
  }
  
  # --- Total yield projection -----------------------------------------------------
  
  if(3 %in% doplot){
    Yd<-MMSE@Catch #MMSE@OM$RefY
    Ydsum<-apply(Yd, c(1,4,5),sum,na.rm=T)
    Ydsum<-Ydsum/array(rep(Ydsum[,,1],MMSE@proyears),dim(Ydsum))
    Ylims<- c(0,quantile(Ydsum,0.95,na.rm=T))
    
    for(pp in 1:length(plots)){
      
      toplot<-plots[[pp]]
      nt<-length(toplot)
      dopar()
      
      for(MP in toplot){
        
        plot(range(yrs),Ylims,col="white",yaxs="i")
        plotquant2(Ydsum[,MP,],p=quants,yrs,qcol,lcol,ablines=c(0.5,1),addline=addline)
        grid()
        mtext(MPrefs[MP,,ss],,3,line=0.2,font=2,cex=0.7)
    
      }
    }
    
    mtext("Projection Year",1,font=2,outer=T,line=xline)
    mtext(paste("Total yield (all fleets) relative to",curyr),2,font=2,outer=T,line=yline)
  }  
  
  # --- Yield by fleet projection --------------------------------------------------
  
  if(4 %in% doplot){
    YdbyF<-apply(Yd, c(3,4,5),mean,na.rm=T)
    cols<-rep(c('black','red','green','blue','orange','grey','purple'),3)
    ltys<-rep(1:3,each=7)
    
    for(pp in 1:length(plots)){
      
      toplot<-plots[[pp]]
      nt<-length(toplot)
      dopar()
      
      for(MP in toplot){
       
          matplot(yrs,t(matrix(YdbyF[,MP,],nrow=nf)),type='l',col="white",lty=ltys,yaxs="i",ylim=c(0,max(YdbyF[,MP,])))
          grid()
          matplot(yrs,t(matrix(YdbyF[,MP,],nrow=nf)),type='l',col=cols,lty=ltys,add=T)
          mtext(MPrefs[MP,,ss],3,line=0.2,font=2,cex=0.7)
          if(MP==toplot[1])legend("topright",legend=paste(paste0("F",1:nf),MMSE@Fnames[,ss]),text.col=cols,cex=0.8,text.font=2,bty='n')
       
      }
    }
    
    mtext("Projection Year",1,font=2,outer=T,line=xline)
    mtext("Expected yield by fleet",2,font=2,outer=T,line=yline)
  }
  
}



getOMs = function(OMdir){
  dirs = list.files(OMdir,full.names=T)
  files = list.files(OMdir)
  OMnam <<- sapply(strsplit(files,".rda"),function(x)x[[1]][1])
  runnam <<-  sapply(strsplit(OMnam,"OM_"),function(x)x[2])
  nOM <<- length(objnam)
  for(i in 1:nOM)  assign(OMnam[i],readRDS(dirs[i]))
  cat(paste("The following OMs have been loaded:",paste(objnam,collapse=", ")," \n"))
  cat(paste("'OMnam','nOM' and 'runnam' also specified \n"))
}


cat("Jack Mackerel to Operating Model (JJ2OM) source code loaded \n")