


TuningCompPlot = function(MSElist,cols = c("red","green","blue","darkgrey")
                          ,adj=0.035,margin=0.075,pos='topright',MSEnams = NA,
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
  
  xlab = "Mean SSB/SSBMSY (last 5 years of projection)"
  if(Bprob) xlab = paste0("P(SSB > ",Blev," SSBMSY (last 5 years of projection)")
  
  plot(xlim,ylim,col="white",ylab = "Mean long-term catch (last 5 years of projection)",
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




cat("Tuning comparison / T-off comparsion code loaded \n")