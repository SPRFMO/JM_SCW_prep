
#x = 1; Data = readRDS("C:/temp/JMdata.rds"); reps = 1; type = "Tbased"

PI = function(x, Data, reps = 1, tunepar = 1, HCR_CP_B = c(0,0),       # increase from zero FMSY to MSY frac from position 1 to position 2. c(0,0) is a constant F policy at MSY frac
              HCR_CP_TAC=c(0,1), type="Fbased"){
  
  ny = max(Data@Year) - Data@LHYear
  FMSY = Data@Misc$ReferencePoints$ReferencePoints$FMSY[x]
  MSY = Data@Misc$ReferencePoints$ReferencePoints$MSY[x]
  VBMSY = Data@Misc$ReferencePoints$ReferencePoints$VBMSY[x]
  UMSY = MSY / (VBMSY + MSY)
  VB = sum(Data@Misc$StockPars$VBiomass_P[x,,ny,]) # use previous year for lag comparability
  MSY = Data@Misc$ReferencePoints$ReferencePoints$MSY[x]
  Brel = Data@Misc$ReferencePoints$ReferencePoints$SSBMSY_SSB0[x]
  Dep = Data@Misc$StockPars$Depletion[x]
  
  if(type =="Fbased")trial_TAC = VB * UMSY * tunepar
  if(type =="Tbased")trial_TAC = MSY * tunepar
  
  Rec = new('Rec')
  Rec@TAC = doHCR(trial_TAC, est=Dep, ref=Brel, CP = HCR_CP_B, CPy = HCR_CP_TAC)
  Rec
  
}

class(PI) = "MP"