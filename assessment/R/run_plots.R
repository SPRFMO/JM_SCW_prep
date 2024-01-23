

modnm <- 

mod <- readJJM(modnm, path = "config", input = "input")
diag <- diagnostics(mod,plot=F)
dev.off()

# names(diag)
plot(diag, var = "ageFitsCatch")
plot(diag, var = "lengthFitsCatch")
plot(diag, var = "ageFitsSurvey")
plot(diag, var = "predictedObservedIndices")
plot(diag, var = "ssbPrediction")
plot(diag, var = "catchPrediction")
