

HCRplot=function(xs=c(0,1),ys=c(0,1),xlab = "SSB / SSBMSY (I / I@SSBMSY)",ps = c(1,0.5),
                 mai = c(0.65,0.65,0.02,0.02)){
  par(mai = mai)
  
  lcols = c("black","darkgrey"); ltys = c(1,3)
  xlim = c(0,1.25); ylim=c(0,1.1)
  
  plot(xlim,ylim,col="white",xlab="",ylab=""); grid(); 
  abline(h=ps,col=lcols,lty=3,lwd=2)
  abline(v=xs[1],col="green",lty=3,lwd=2)
  abline(h=ys[1],col="red",lty=3,lwd=2)
  
  for(pp in 1:length(ps)){
    lines(c(0,xs[1]),rep(ys[1]*ps[pp],2),col=lcols[pp],lwd=3,lty=ltys[pp]); 
    lines(xs,ys*ps[pp],col=lcols[pp],lwd=3,lty=ltys[pp])
    lines(c(xs[2],1E10),rep(ys[2]*ps[pp],2),col=lcols[pp],lwd=3,lty=ltys[pp])
  }
  mtext(xlab,1,line=2.2,outer=F)
  mtext("Fmod",2,line=2.2,outer=F)
}


  

