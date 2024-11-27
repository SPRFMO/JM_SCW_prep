
# A default plot builder for standardizing report figure dimensions and resolution

doFig<-function(expr, file = NULL, width=9.5, height=3.1, res=400, units='in',type='jpeg',ggplot=F){
  
  if(is.null(file))stop("You need to specify the file argument - a file location and name (without extension), e.g. C:/Temp/testfig")
  if(type=="jpeg")filename<-paste0(file,".jpg")
  if(type=="png")filename<-paste0(file,".png")
  
  if(ggplot){
    dev.new()
    expr
    ggsave(filename,width=width,height=height,units=units,dpi=res)
    dev.off()
  }else{
    do.call(type,list(filename=filename,width=width,height=height,res=res,units=units))
    expr
    dev.off()
  }
  
}
