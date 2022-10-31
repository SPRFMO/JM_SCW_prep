#--------------------------
# Generate summary table for historical retro
#--------------------------
library(jjmR)

FinModName <- "1.02"

mod_final <- readJJM(paste0("h1_",FinModName),path="config",input="input")   # current year assessment
repfile <- mod_final[[1]]$output[[1]]
assessmenttype <- "assessment"

table_old <- read.csv("hist_retro/SPRFMO historical retro.csv",header = T)
vars2pull <- colnames(table_old)
Nvars <- length(vars2pull)
Nyrs <- length(repfile$Yr)
sumtable <- matrix(NA, nrow = Nyrs, ncol = Nvars)
colnames(sumtable) <- vars2pull

table_old<-table_old[!table_old[,"Assessmentyear"] %in% max(repfile$Yr),]

sumtable[,"Assessmentyear"] <- max(repfile$Yr)
sumtable[,"Year"] <- repfile$Yr
sumtable[,grep("SSB", vars2pull)] <- repfile$SSB[repfile$SSB[,1] %in% repfile$Yr,2:5]
sumtable[,grep("R", vars2pull)] <- repfile$R[repfile$R[,1] %in% repfile$Yr,2:5]
sumtable[,grep("F1",vars2pull)[1]:grep("F12",vars2pull)] <- repfile$TotF
sumtable[,"F.Fmsy"] <- repfile$msy_mt[,4]
sumtable[,"Fmsy"] <- repfile$msy_mt[,5]
sumtable[,"Favg"] <- repfile$msy_mt[,6]
sumtable[,"Bmsy"] <- repfile$msy_mt[,10]
sumtable[,"B.Bmsy"] <- repfile$msy_mt[,13]
sumtable[,"assessmenttype"] <- assessmenttype

# if current assessment is larger than data contained in the 
if(max(table_old$Assessmentyear)>=max(repfile$Yr) & assessmenttype == "assessment") {
  temp <- table_old[-which(table_old$Assessmentyear == max(table_old$Assessmentyear)),]
  table_new <- rbind(temp, sumtable)
}

if(max(table_old$Assessmentyear)<max(repfile$Yr) | assessmenttype != "assessment") {
  table_new <- rbind(table_old, sumtable)
}

write.csv(as.data.frame(table_new), file = "SPRFMO historical retro.csv", quote = FALSE,row.names = F)

