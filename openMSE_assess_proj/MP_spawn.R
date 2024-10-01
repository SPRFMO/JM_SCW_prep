

# Spawns multiple versions of an MP MPname based on a set of optional named defaults, a design grid spawnlist and assigns these names spawnnnames

# setwd("C:/GitHub/MSE-GFCM/BS_Rapa/R_MP_development") 
# setwd("C:/Users/tcarruth/Documents/GitHub/MSE-GFCM/BS_Rapa/R_MP_development")
#largedir = "C:/temp/GFCM/RapaMP"
#library(openMSE)
#library(spict)
#library(miceadds)
#library(dplyr)
#MSElist = rep(list(readRDS(paste0(largedir,"/testMSE.rds"))),2)
#names(MSElist) = c("Test1","Test2")


spawnMP = function(MPname,defaults=list(NA),spawnlist,spawnnames = NA){
  
  classy = class(get(MPname))
  template = get(MPname)
  forms = names(formals(template))
  
  if(!all(names(defaults)%in%forms))stop(paste("At least one of the named defaults is not a formal of MP",MPname))
  
  if(!is.na(defaults[[1]][1])){ # only if there are defaults
    for(i in 1:length(defaults))formals(template)[names(defaults)[i]]=defaults[i] # assign new defaults
  }
  
  if(is.na(spawnnames[1])){
    combos = lapply(spawnlist)
    spawnnames = paste(MPname,names(spawnlist)[1],spawnlist[[1]],sep="_")
    cat(paste0("Inventing spawnnames (you might want to add an argument):", paste(spawnnames,collapse=", "),"/n"))
  }
  
  spawncheck = sapply(spawnlist,length)
  if(!all(spawncheck==mean(spawncheck)))stop(paste("At least one of the named defaults is not a formal of MP",MPname))
  
  ns = length(spawnlist[[1]])
  nt = length(spawnlist)
  
  for(i in 1:ns){ # levels of argument
    
    temp2 = template
    
    for(tt in 1:nt){ # types of argument
    
      formchng = names(spawnlist)[tt]
      if(!(formchng%in%forms))stop(paste("The spawnlist name",formchng, "does not match a formal of formal of MP",MPname))
      formals(temp2)[formchng] = spawnlist[[tt]][i]
    
      if(tt ==nt){ # final assignment
        class(temp2) = classy
        assign(spawnnames[i],temp2,envir=.GlobalEnv)
      }
    } # end of type
  } # end of spawn
  
}


