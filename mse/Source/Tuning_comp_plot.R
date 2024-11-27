


TuningCompPlot = function(MSElist,cols = c("red","green","blue","darkgrey","orange"),
                          adj=0.035,margin=0.075,pos='topright',MSEnams = NA,
                          Bprob = FALSE, Blev=0.5){
  
  MSE1 = MSElist[[1]]
  if(is.na(MSEnams[1]))MSEnams = paste("MSE",1:length(MSElist))
  nMSE = length(MSElist)
  nyears = MSE1@nyears
  proyears = MSE1@proyears
  
  Brel = lapply(MSElist,function(X)apply(X@SB_SBMSY[,,proyears-0:4],2,mean))
  if(Bprob)Brel = lapply(MSElist,function(X)apply(X@SB_SBMSY[,,proyears-0:4]>Blev,2,mean))
  Cat = lapply(MSElist,function(X)apply(X@Catch[,,proyears-0:4],2,mean))
  
  xlim = range(unlist(Brel))
  ylim = range(unlist(Cat))
  xlim = xlim + c(-margin,margin)*(xlim[2]-xlim[1])
  ylim = ylim + c(-margin,margin)*(ylim[2]-ylim[1])
  
  xlab = "Mean SSB/SSBMSY (last 5 proj. yrs)"
  if(Bprob) xlab = paste0("P(SSB > ",Blev," SSBMSY (last 5 proj. yrs)")
  
  plot(xlim,ylim,col="white",ylab = "Mean long-term catch (last 5 proj. yrs)",
                             xlab = xlab);grid()
 
  for(mm in 1:nMSE){
    coly = makeTransparent(cols[mm],90)
    points(Brel[[mm]],Cat[[mm]],col=coly,pch=19)
    ord=order(Brel[[mm]])
    lines(Brel[[mm]][ord],Cat[[mm]][ord],col = coly, lwd=2)
    text(Brel[[mm]]+adj,Cat[[mm]]+adj,MSElist[[mm]]@MPs,cex=0.8,font=2,col=coly)
  }
  
  legend(pos,legend=MSEnams,text.col=cols,bty="n")
  
}


TCPplot = function(vx,vy,xlab,ylab,adj=0.03,yzero=F,cols){
  nMSE = length(vx)
  xlim = range(unlist(vx))
  ylim = range(unlist(vy))
  if(yzero)ylim=c(0,ylim[2])
  xlim = xlim + c(-margin,margin)*(xlim[2]-xlim[1])
  ylim = ylim + c(-margin,margin)*(ylim[2]-ylim[1])
  xrng = xlim[2] - xlim[1]
  yrng = ylim[2] - ylim[1]
  xadj = xrng * adj
  yadj = yrng * adj
  plot(xlim,ylim,col="white",ylab = "", xlab = "");grid()
  mtext(xlab,1,line=2.1,cex=0.6,font=2); mtext(ylab,2,line=2.1,cex=0.6,font=2)
  
  for(mm in 1:nMSE){
    coly = makeTransparent(cols[mm],90)
    points(vx[[mm]],vy[[mm]],col=coly,pch=19)
    ord=order(vx[[mm]])
    lines(vx[[mm]][ord],vy[[mm]][ord],col = coly, lwd=2)
    text(vx[[mm]]+xadj,vy[[mm]]+yadj,MSElist[[mm]]@MPs,cex=0.8,font=2,col=coly)
  }
}

TCP2 = function(Mlist, MSEnams = NA, title="", cols = c("red","darkgrey","green","blue","orange"),
                          margin=0.075, pos='topright',
                          Bprob = FALSE, Blev=1,mfrow=c(1,4),
                          yzero = F,cind = NA){
  
  par(mfrow=mfrow,mai=c(0.5,0.4,0.05,0.05),omi=c(0.01,0.01,0.2,0.025))
  MSE1 = Mlist[[1]]
  if(is.na(MSEnams[1]))MSEnams = paste("MSE",1:length(Mlist))
  nMSE = length(Mlist)
  if(is.na(cind[1]))cind = 1:nMSE
  nyears = MSE1@nyears
  proyears = MSE1@proyears
  
  Brel = lapply(Mlist,function(X)apply(X@SB_SBMSY[,,proyears-0:4],2,mean))
  Bprob = lapply(Mlist,function(X)apply(X@SB_SBMSY[,,proyears-0:4]>1,2,mean))
  Cat = lapply(Mlist,function(X)apply(X@Catch[,,proyears-0:4],2,mean))
  Brel10 = lapply(Mlist,function(X)apply(X@SB_SBMSY[,,proyears-0:4],2,quantile,p=0.1))
  Cat10 = lapply(Mlist,function(X)apply(X@Catch[,,proyears-0:4],2,quantile,p=0.1))
  CatST = lapply(Mlist,function(X)apply(X@Catch[,,1:10],2,mean))
 
  varfunc = function(X, interval = 2, maxVY = 0.5){ # MPs are constrained to max var 0.5 - this just stops rare crashed stocks (Zero catch) from affecting VY calc
    proyears = X@proyears
    ys = 1:proyears*interval
    ind1 = ys[1:(proyears-1)]; ind1 = ind1[ind1<proyears]; ni=length(ind1)
    ind2 = ys[2:proyears][1:ni]
    dif = X@Catch[,,ind2]-X@Catch[,,ind1]
    VY = abs(dif/X@Catch[,,ind1])
    VY[VY > maxVY] = maxVY
    VY
  }  
  VY = lapply(Mlist,function(X)-(apply(varfunc(X),2,mean)))
  
  # Brel Cath
  #xlab = "Mean SSB/SSBMSY (last 5 proj. yrs)"
  #if(Bprob) xlab = paste0("P(SSB > ",Blev," SSBMSY (last 5 proj. yrs)")
  
  TCPplot(Brel,Cat,xlab = "Mean SSB/SSBMSY (last 5 yrs)", ylab = "Mean Catch (last 5 yrs)", yzero=yzero, cols=cols[cind])
  legend(pos,legend=MSEnams,text.col=cols[cind],bty="n",text.font=2)
  TCPplot(Brel10,Cat10,xlab = "10th% SSB/SSBMSY (last 5 yrs)", ylab = "10th% Catch (last 5 yrs)", yzero=yzero, cols=cols[cind])
  TCPplot(CatST,Cat,xlab = "Mean Catch (first 10 yrs)", ylab = "Mean Catch (last 5 yrs)", yzero=yzero, cols=cols[cind])
  TCPplot(Brel, VY,"Mean SSB/SSBMSY (last 5 yrs)", "Variability in yield (abs, neg.)", cols=cols[cind])
  mtext(title, line=0.05, cex=0.8, font=2, outer=T)
  
}


cat("Tuning comparison / T-off comparsion code loaded \n")