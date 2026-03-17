library(jjmR)

modelName = "model_name"
  
runJJM(modelName, path="config", input="input", exec="src/jjm", output = "results/")
runit(modelName, est=FALSE, portrait=F, pdf=TRUE)

