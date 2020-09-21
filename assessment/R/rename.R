#mod1.0 <- readJJM("mod1.0", path = "config", input="input")
#mod1.4 <- readJJM("mod1.4", path = "config", input="input")
#Mods <- combineModels(mod1.0,mod1.4)

changeNameModel = function(modList, nameVector){
  for(i in seq_along(modList)){
    modList[[i]]$info$output$model <- nameVector[i]
  }
  return(modList)
}

#Mods <- changeNameModel(Mods,c("modelo1","modelo2"))
#plot(Mods)
