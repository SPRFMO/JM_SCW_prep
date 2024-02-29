
# Source code for converting Jim's Jack Mackerel MLE and MCMC output into an OpenMSE operating model

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

plotJMcomp=function(mod,naa,faa,Hist,simnos = 1:2){
  
  est0 = mod[[1]]$output$Stock_1
  F_est = est0$TotF
  N_est = est0$N
  B_est = apply(est0$TotBiom[,2:5],1,sum)
 
  F_sim = Hist@AtAge$F.Mortality[,,,1]            # mixed model so area 1 is indicative
  N_sim = apply(Hist@AtAge$Number,1:3,sum)        # sum over areas
  B_sim = apply(Hist@TSdata$Biomass,1:2,sum)
  yrs = Hist@Data@Year
  
  par(mfrow=c(4,2),mai=c(0.3,0.4,0.3,0.01))
  # F 
  age = 3; 
  docomp(yrs,est=faa[simnos[1],age+1,],sim = F_sim[simnos[1],age+1,],paste("Age",age,"F, sim ",simnos[1]))
  legend('topleft',legend=c("JMest","openMSE"),text.col=c("blue",'red'),bty="n",cex=0.8)
  docomp(yrs,est=faa[simnos[2],age+1,],sim = F_sim[simnos[2],age+1,],paste("Age",age,"F, sim ",simnos[2]))
  
  age = 8; 
  docomp(yrs,est=faa[simnos[1],age+1,],sim = F_sim[simnos[1],age+1,],paste("Age",age,"F, sim ",simnos[1]))
  legend('topleft',legend=c("JMest","openMSE"),text.col=c("blue",'red'),bty="n",cex=0.8)
  docomp(yrs,est=faa[simnos[2],age+1,],sim = F_sim[simnos[2],age+1,],paste("Age",age,"F, sim ",simnos[2]))
  
  
  # N 
  age = 1
  docomp(yrs,est=naa[simnos[1],age+1,],sim = N_sim[simnos[1],age+1,],paste("Age",age,"Numbers, sim ",simnos[1]))
  legend('topleft',legend=c("JMest (end of year)","openMSE (start of year)"),text.col=c("blue",'red'),bty="n",cex=0.8)
  docomp(yrs,est=naa[simnos[2],age+1,],sim = N_sim[simnos[2],age+1,],paste("Age",age,"Numbers, sim ",simnos[2]))

  age = 5
  docomp(yrs,est=naa[simnos[1],age+1,],sim = N_sim[simnos[1],age+1,],paste("Age",age,"Numbers, sim ",simnos[1]))
  legend('topleft',legend=c("JMest (end of year)","openMSE (start of year)"),text.col=c("blue",'red'),bty="n",cex=0.8)
  docomp(yrs,est=naa[simnos[2],age+1,],sim = N_sim[simnos[2],age+1,],paste("Age",age,"Numbers, sim ",simnos[2]))
  
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
  naa[,1,] = naa[,2,] * exp(rep(MLE$M[1:nyears,1],each=nsim)) ; cat("Age 0 numbers are imputed based on age 1 numbers and natural mortality rate \n")
  
  faa = make_age_array(mcmc, "F_stock"); cat("Fishing mortality rate is assumed to be zero for age 0 fish \n")
  faa[faa==0] = 1E-3
  waa = array(rep(c(0,MLE$wt_a_pop),each=nsim),c(nsim,nage+1,nyears)); cat("wt_a_pop used. Age zero fish are assumed to have weight zero \n") 
  Mataa = array(rep(c(0,MLE$mature_a),each=nsim),c(nsim,nage+1,nyears)); cat("mature_a used. Maturity = 0 is assumed for age zero fish \n") 
  Maa = abind(matrix(rep(MLE$M[1:nyears,1],each=nsim),nrow=nsim),array(rep(t(MLE$M[1:nyears,]),each=nsim),c(nsim,nage,nyears)),along=2); cat("Age zero fish assumed to have same natural mortality rate as age 1 fish \n")
  len_a = cn$Lo_len[1,1] + cn$Linf[1,1] * (1-exp(-(0:nage)*cn$K[1,1]))
  laa = array(rep(len_a,each=nsim), c(nsim,nage+1,nyears)); cat("Somatic growth assumed to be Lo_len + Linf * (1-exp(-ak)) \n")
  
  #waa = array(rep(MLE$wt_a_pop,each=nsim),c(nsim,nage,nyears)); cat("wt_a_pop used. Age zero fish are assumed to have weight zero \n") 
  #Mataa = array(rep(MLE$mature_a,each=nsim),c(nsim,nage,nyears)); cat("mature_a used. Maturity = 0 is assumed for age zero fish \n") 
  #Maa = array(rep(t(MLE$M[1:nyears,]),each=nsim),c(nsim,nage,nyears)); cat("Age zero fish assumed to have same natural mortality rate as age 1 fish \n")
  #len_a = cn$Lo_len[1,1] + cn$Linf[1,1] * (1-exp(-(1:nage)*cn$K[1,1]))
  #laa = array(rep(len_a,each=nsim), c(nsim,nage,nyears)); cat("Somatic growth assumed to be Lo_len + Linf * (1-exp(-ak)) \n")
  
  
  
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

cat("Jack Mackerel to Operating Model (JJ2OM) source code loaded \n")