# MP tuning

Btuning = function(MSEobj,Btarg=1,xs = seq(0.3,0.9,by=0.1),yr = 50,plot=T){
  Best = apply(MSEobj@SB_SBMSY[,,yr],2,mean)
  xtarg = approx(Best,xs,Btarg)$y
  if(plot){plot(xs,Best);abline(h=Btarg,v=xtarg,col="red")}
  xtarg
}