
Like=cbind( summary(mod1.0)$like,  summary(mod1.3)$like,  summary(mod1.11)$like,  summary(mod1.14)$like )

  summary(mod0.1ms2)$like, summary(mod0.2ms2)$like,
           summary(mod0.3ms2)$like, summary(mod0.4ms2)$like)
Like=cbind(summary(mod0.0ms2)$like, summary(mod0.1ms2)$like, summary(mod0.2ms2)$like,
           summary(mod0.3ms2)$like, summary(mod0.4ms2)$like)

Like=cbind(summary(mod0.2ms2)$like, summary(mod0.4ms2)$like)
Like
model=mod1.11
plots=diagnostics(model)
plots[[1]]$Stock_1$output$totalCatchByFleet

models = combineModels(mod1.0,mod1.3,mod1.11,mod1.14,modx.3)
plot(models, what = "ssb", main = "",stack=F, combine = T)
plot(models, what = "recruitment", main = "",stack=F, combine = T)
plot(models, what = "recruitment", main = "",stack=F, combine = T,ylim=c(0,1.4e5))

model=mod1.11
model=mod1.14
plot(model, what = "ssb", main = "",stack=F, combine = T)
plot(model, what = "recruitment", main = "",stack=F, combine = T)
plots=diagnostics(model)
plots[[1]]$Stock_1$data$weightFishery
plots[[1]]$Stock_2$data$weightFishery
plots[[1]]$Stock_1$output$ageFitsCatch
plots[[1]]$Stock_2$output$lengthFitsCatch
plots[[1]]$Stock_1$output$ageFitsSurvey
plots[[1]]$Stock_1$output$predictedObservedIndices
plots[[1]]$Stock_2$output$predictedObservedIndices
plots[[1]]$Stock_1$output$fisheryMeanAge
plots[[1]]$Stock_1$output$surveyMeanAge
plots[[1]]$Stock_1$output$selectivityFisheryByPentad
plots[[1]]$Stock_2$output$selectivityFisheryByPentad
  plots[[1]]$Stock_1$output$summarySheet
plots[[1]]$Stock_2$output$summarySheet
plots[[1]]$Stock_1$output$fishedUnfishedBiomass
plots[[1]]$Stock_2$output$fishedUnfishedBiomass
plots[[1]]$Stock_1$output$kobePlot
plots[[1]]$Stock_2$output$kobePlot
plots[[1]]$Stock_1$output$ssbPrediction
plot.jjm.output(model, what = "totalProj")

png("A4.1.png", width = 900, height = 700, bg = "transparent")
dev.off()
#
