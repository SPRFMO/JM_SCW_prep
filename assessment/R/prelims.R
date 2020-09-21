mytheme <- theme(panel.grid.major.x = element_blank(), panel.grid.minor.y = element_blank(), panel.grid.major.y = element_blank() )
mytheme <- mytheme + theme(text=element_text(size=18)) + theme(axis.title.x=element_text(size=24) ,axis.title.y=element_text(size=24))
mytheme <- mytheme + theme(panel.grid.major.x = element_blank(), panel.grid.minor.x = element_blank(),panel.grid.minor.y = element_line(colour="grey60", linetype="dashed"), panel.grid.major.y = element_blank() )
mytheme <- mytheme + theme( panel.background = element_rect(fill="white"), panel.border = element_rect(colour="black", fill=NA, size=1))

#runit <- function(mod="",est=F,portrait=T,exec="jjms",version="2014",pdf=F){
	#if (est)
    #runJJM(mod, path="config", input="input", version=version,exec=exec)
  #modtmp <- readJJM(mod, path="config", input="input", version="2014")
  #if(pdf){
		#if (portrait)
    	#pdf(paste0("results/",mod,".pdf"),height=9,width=7)
  	#else
    	#pdf(paste0("results/",mod,".pdf"),height=7,width=9)
  	#plot(diagnostics(modtmp))
  	#dev.off()
  	#}
  #return(modtmp)
#}
