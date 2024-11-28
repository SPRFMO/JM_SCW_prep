# ===========================================================================================================
# === Script to run the specified MPs against all OMs =======================================================
# ===========================================================================================================

# Tom Carruthers
# November 2024

# --- Prerequisites ------------------------------------------------------------

setwd("C:/GitHub/jjm/assessment") # Tom WD #setwd("C:/Users/tcarruth/Documents/GitHub/jjm/assessment") # Tom WD

library(tidyverse)
library(flextable)
library(libr)
library(fmtr)
library(abind)
library(data.table)
library(openMSE)
library(miceadds)

source.all("../mse/MPs")               # load MP building
source.all("../mse/Source") 
largedir = "C:/Users/tcar_/Dropbox/temp/JJM_MPtest" #largedir = "C:/Users/tcarruth/Dropbox/temp/JJM_MPtest"
figdir = "C:/Users/tcar_/Dropbox/temp/JJM_Figs/" #largedir = "C:/Users/tcarruth/Dropbox/temp/JJM_MPtest"


# --- Conditioning Model inputs / outputs --------------------------------------

GetOM_Jim <- function(run_loc, hyp="h1_ls", nsim=128) {
  mod <- jjmR::readJJM(hyp, output = run_loc, path = run_loc, input = run_loc)
  mceval_file = paste0(run_loc, "/mceval.csv" ) # location of MCMC output of F (by fleet) and N at age
  return (JM2MOM(mod, mceval_file,
                 proyears = 50,          # 50 projection years
                 interval = 2,           # update management every 2 years
                 nsim = nsim,            # sims
                 datastart = 2007,       # only the last 15 years of data are submitted to derive statistical properties for index simulation
                 returnMOM = F,          # aggregate fleets (makes no difference it seems)
                 check = F)              # visual check that reconstructed openMSE N and F matches conditioning model estimates
  )
}

hyp = c("h1_ls","h1_ll","h1_hl","h1_hs")
nsim = 16; 
sims = c(16, 48, 96, 192)
for(ss in 1:length(sims)){
  for(i in 1:4){
    OM = GetOM_Jim(paste0(largedir,"/mcmc/om",i,"/mcmc"), hyp[i],nsim=sims[ss])
    Hist = runMSE(OM, Hist = T)
    saveRDS(Hist, paste0(largedir,"/Hist_",i,"_",sims[ss],".rda"))
  }
}


# --- Make MPs -----------------------------------------------------------------

source.all('../mse/MPs/')

ntune = 16
Fseq = seq(0.3,1.8,length.out=ntune)
Flab = Fseq*10

make_s_list = function(B1=0 , B2=1 , F1=1, F2=1, Fseq = seq(0.5,1.2,length.out=8),
                       BSD = 0.15, BAC = 0, Bbias = 1 ){
  ntune = length(Fseq)
  list(B1 = rep(B1,ntune), B2 = rep(B2,ntune), F1 = F1*Fseq, F2 = F2*Fseq, 
       BSD = rep(BSD,ntune), BAC = rep(BAC,ntune), Bbias = rep(Bbias,ntune))
} 

# --- Short cut good BSD = 0.15, BAC = 0, Bbias = 1
Sg_01 = paste0("Sg_01_",Flab); spawnSC(make_s_list(Fseq = Fseq), Sg_01)                 # constant F (HCR 0,1  1,1 )
Sg_05 = paste0("Sg_05_",Flab); spawnSC(make_s_list(F1=0.5,Fseq = Fseq), Sg_05)           # HCR 0,1  0.5,1 
Sg_00 = paste0("Sg_00_",Flab); spawnSC(make_s_list(F1=0,Fseq = Fseq), Sg_00 )            # HCR 0,1  0,1 
Sg_55 = paste0("Sg_55_",Flab); spawnSC(make_s_list(F1=0.5, B1 = 0.5,Fseq = Fseq), Sg_55) # HCR 0.5,1  0.5,1 
Sg_50 = paste0("Sg_50_",Flab); spawnSC(make_s_list(F1=0, B1 = 0.5,Fseq = Fseq), Sg_50)   # HCR 0.5,1  0.5,1 

#  --- Short cut moderate BSD = 0.2, BAC = 0.6, Bbias = 1
Sm_01 = paste0("Sm_01_",Flab); spawnSC(make_s_list(BSD=0.2, BAC=0.6, Fseq = Fseq), Sm_01)                   # constant F (HCR 0,1  1,1 )
Sm_05 = paste0("Sm_05_",Flab); spawnSC(make_s_list(BSD=0.2, BAC=0.6, F1=0.5,Fseq = Fseq), Sm_05)           # HCR 0,1  0.5,1 
Sm_00 = paste0("Sm_00_",Flab); spawnSC(make_s_list(BSD=0.2, BAC=0.6, F1=0,Fseq = Fseq), Sm_00)             # HCR 0,1  0,1 
Sm_55 = paste0("Sm_55_",Flab); spawnSC(make_s_list(BSD=0.2, BAC=0.6, F1=0.5, B1 = 0.5,Fseq = Fseq), Sm_55) # HCR 0.5,1  0.5,1 
Sm_50 = paste0("Sm_50_",Flab); spawnSC(make_s_list(BSD=0.2, BAC=0.6, F1=0, B1 = 0.5,Fseq = Fseq), Sm_50)   # HCR 0.5,1  0.5,1 

# --- Short cut bad BSD = 0.25, BAC = 0.75, Bbias = 1.25
Sb_01 = paste0("Sb_01_",Flab); spawnSC(make_s_list(BSD=0.25, BAC=0.75, Bbias=1.15, Fseq = Fseq), Sb_01)                   # constant F (HCR 0,1  1,1 )
Sb_05 = paste0("Sb_05_",Flab); spawnSC(make_s_list(BSD=0.25, BAC=0.75, Bbias=1.15, F1=0.5,Fseq = Fseq), Sb_05)           # HCR 0,1  0.5,1 
Sb_00 = paste0("Sb_00_",Flab); spawnSC(make_s_list(BSD=0.25, BAC=0.75, Bbias=1.15, F1=0,Fseq = Fseq), Sb_00)             # HCR 0,1  0,1 
Sb_55 = paste0("Sb_55_",Flab); spawnSC(make_s_list(BSD=0.25, BAC=0.75, Bbias=1.15, F1=0.5, B1 = 0.5,Fseq = Fseq), Sb_55) # HCR 0.5,1  0.5,1 
Sb_50 = paste0("Sb_50_",Flab); spawnSC(make_s_list(BSD=0.25, BAC=0.75, Bbias=1.15, F1=0, B1 = 0.5,Fseq = Fseq), Sb_50)   # HCR 0.5,1  0.5,1 


# --- Assessment via rapid SCA 

make_a_list = function(B1=0 , B2=1 , F1=1, F2=1, Fseq = seq(0.5,1.2,length.out=8)){
  ntune = length(Fseq)
  list(B1 = rep(B1,ntune), B2 = rep(B2,ntune), F1 = F1*Fseq, F2 = F2*Fseq)
} 

A_01 = paste0("A_01_",Flab); spawnSC(make_a_list(Fseq = Fseq), A_01)                 # constant F (HCR 0,1  1,1 )
A_05 = paste0("A_05_",Flab); spawnSC(make_a_list(F1=0.5,Fseq = Fseq), A_05)           # HCR 0,1  0.5,1 
A_00 = paste0("A_00_",Flab); spawnSC(make_a_list(F1=0,Fseq = Fseq), A_00 )            # HCR 0,1  0,1 
A_55 = paste0("A_55_",Flab); spawnSC(make_a_list(F1=0.5, B1 = 0.5,Fseq = Fseq), A_55) # HCR 0.5,1  0.5,1 
A_50 = paste0("A_50_",Flab); spawnSC(make_a_list(F1=0, B1 = 0.5,Fseq = Fseq), A_50)   # HCR 0.5,1  0.5,1 
  

# --- Empirical 

Ifacs = seq(0.4, 3.4, length.out=ntune)
Ilab = Ifacs*10
spawnlist = list(Ind_fac = Ifacs)

E_01 = paste0("E_01_",Ilab); spawnMP('Emp', defaults=list(HCR_CP_B = c(0,1),   HCR_CP_TAC=c(1,1)),   spawnlist, E_01) # constant F (HCR 0,1  1,1 )
E_05 = paste0("E_05_",Ilab); spawnMP('Emp', defaults=list(HCR_CP_B = c(0,1),   HCR_CP_TAC=c(0.5,1)), spawnlist, E_05) # HCR 0,1  0.5,1 
E_00 = paste0("E_00_",Ilab); spawnMP('Emp', defaults=list(HCR_CP_B = c(0,1),   HCR_CP_TAC=c(0,1)),   spawnlist, E_00) # HCR 0,1  0,1 
E_55 = paste0("E_55_",Ilab); spawnMP('Emp', defaults=list(HCR_CP_B = c(0.5,1), HCR_CP_TAC=c(0.5,1)), spawnlist, E_55) # HCR 0.5,1  0.5,1 
E_50 = paste0("E_50_",Ilab); spawnMP('Emp', defaults=list(HCR_CP_B = c(0.5,1), HCR_CP_TAC=c(0,1)),   spawnlist, E_50) # HCR 0.5,1  0.5,1 
  

# --- TAC-based PI 

spawnlist = list(tunepar = Fseq)
TB_00 = paste0("TB_00_",Flab); spawnMP('PI', defaults=list(HCR_CP_B = c(0,1),   HCR_CP_TAC=c(0,1), type = "Tbased"),   spawnlist, TB_00) # HCR 0,1  0,1 


# --- F-based PI 

FB_00 = paste0("FB_00_",Flab); spawnMP('PI', defaults=list(HCR_CP_B = c(0,1),   HCR_CP_TAC=c(0,1), type = "Fbased"),   spawnlist, FB_00) # HCR 0,1  0,1 
FB_01 = paste0("FB_01_",Flab); spawnMP('PI', defaults=list(HCR_CP_B = c(0,1),   HCR_CP_TAC=c(1,1), type = "Fbased"),   spawnlist, FB_01) # HCR 0,1  1,1 



# --- Calculations -------------------------------------------------------------

# --- full sets 

doruns = function(OMnams,ext="16",MPind=NA){
  MPsets = paste0(rep(c("Sg","Sm","Sb","A","E"),each=5),"_",c("01","05","00","55","50"))
  
  if(is.na(MPind[1]))MPind = 1:length(MPsets) #MPind = grep("01",MPsets) MPind = match(c("Sm_55","E_50"), MPsets)
  setup()
  for(om in 1:length(OMnams)){
    Hist = readRDS(paste0(largedir,"/", OMnams[om]))
    for(i in MPind){
      MSEnam = paste0("MSE_",MPsets[i],"_",om,"_",ext)
      temp = Project_parallel(Hist, get(MPsets[i]))
      assign(MSEnam, mergeMPs(temp))
      saveRDS(get(MSEnam), paste0(largedir,"/",MSEnam,".rds"))
}}}    

 
doruns(OMnams = c("Hist_1_16.rda", "Hist_3_16.rda"), ext="16")  # 16 sim
doruns(OMnams = c("Hist_1_48.rda", "Hist_3_48.rda"), ext="48")  # 48 sim
doruns(OMnams = c("Hist_1_96.rda", "Hist_3_96.rda"), ext="96")  # 96 sim

MPind =  match(c("Sg_00","A_00","E_00"), paste0(rep(c("Sg","Sm","Sb","A","E"),each=5),"_",c("01","05","00","55","50")))
doruns(OMnams = "Hist_1_192.rda",MPind = MPind,ext="192") # 192 sim

# F vs TAC types OM1 96 sim, limited comparison

Hist =  readRDS(paste0(largedir,"/Hist_1_96.rda"))
MSE = mergeMPs(Project_parallel(Hist, TB_00))
saveRDS(MSE, paste0(largedir,"/MSE_TB_00_1_96.rds"))

MSE2 = mergeMPs(Project_parallel(Hist, FB_00))
saveRDS(MSE2, paste0(largedir,"/MSE_FB_00_1_96.rds"))

MSE3 = mergeMPs(Project_parallel(Hist, FB_01))
saveRDS(MSE3, paste0(largedir,"/MSE_FB_01_1_96.rds"))


# --- plot HCRs ----------------------------------------------------------------

HCRspec = data.frame(x1 = c(0,0,0,0.5,0.5), y1 = c(1, 0.5, 0, 0.5, 0))
HCRnam = c("01","05","00","55","50")
for(i in 1:length(HCRnam))doFig(HCRplot(c(HCRspec$x[i],1),c(HCRspec$y[i],1)),paste0(figdir,"HCR_",HCRnam[i]),width=4,height=2.5)
for(i in 1:length(HCRnam))doFig(HCRplot(c(HCRspec$x[i],1),c(HCRspec$y[i],1),mai=rep(0.025,4)),paste0(figdir,"HCR_ruleonly_",HCRnam[i]),width=3,height=3)

  
# --- consolidate results -------------------------------------------------------------

ntune=16
MPsets = paste0(rep(c("Sg","Sm","Sb","A","E"),each=5),"_",c("01","05","00","55","50"))
MPind = 1:length(MPsets)

#            short cut and assessment MPs    Empirical 
mplabs =list(seq(0.3,1.8,length.out=ntune),seq(0.4, 3.4, length.out=ntune))
ind = rep(1,length(MPind)); ind[grepl("E",MPsets)]=2

MSElist=list(); j=0
for(i in MPind){
  j = j+1
  MSEnam = paste0("MSE_",MPsets[i],"_1_96")
  MSE = readRDS(paste0(largedir,"/",MSEnam,".rds"))
  MSE@MPs = as.character(mplabs[[ind[i]]])
  MSElist[[j]] = MSE
}


# --- plot results -------------------------------------------------------------



MSEt = Sub(MSElist[[1]],MPs=c("0.3","1.8"))
doFig(Splot(MSEt,ymaxs = c(NA,3,2000)),paste0(figdir,"Fig 0 PM explanation"),height=6.5)


# short cuts vs others 
indCF = grep("01",MPsets)
ind = indCF[1:3]
doFig(TCP2(Mlist = MSElist[ind], MPsets[ind],"Constant F policy - Short cuts"),paste0(figdir,"Fig 1 Const F shortcut"))
doFig(TCP2(Mlist = MSElist[ind], MPsets[ind],"Constant F policy - Short cuts",yzero=T),paste0(figdir,"Fig 2 Const F shortcut"))

ind = indCF[1:4]
doFig(TCP2(Mlist = MSElist[ind], MPsets[ind],"Constant F policy - Short cuts"),paste0(figdir,"Fig 3 Const F shortcut vs SCA"))

subi = c(1,3,4,5)
ind = indCF[subi]
doFig(TCP2(Mlist = MSElist[ind], MPsets[ind],"Constant F policy - Short cuts",cind=subi),paste0(figdir,"Fig 4 Const F shortcut vs SCA vs Emp"))
doFig(TCP2(Mlist = MSElist[ind], MPsets[ind],"Constant F policy - Short cuts",yzero=T,cind=subi),paste0(figdir,"Fig 5 Const F shortcut vs SCA vs Emp yzero"))

# short cut HCRs
indHCR = grep("Sm",MPsets)
doFig(TCP2(Mlist = MSElist[indHCR], MPsets[indHCR],"Short cut moderate (Sm) derivatives",yzero=T),paste0(figdir,"Fig 6 Sg HCR"))

# assessment HCRs
indHCR = grep("A",MPsets)
doFig(TCP2(Mlist = MSElist[indHCR], MPsets[indHCR],"SCA assessment (A) derivatives",yzero=T),paste0(figdir,"Fig 7 SCA HCR"))

# Empirical HCRs
indHCR = grep("E",MPsets)
doFig(TCP2(Mlist = MSElist[indHCR], MPsets[indHCR],"Empirical (E) derivatives",yzero=T),paste0(figdir,"Fig 8 Emp HCR"))


# Effect of simno
MSElist_sim=list()
MSElist_sim[[1]] = readRDS(paste0(largedir,"/MSE_E_50_1_16.rds"))
MSElist_sim[[2]] = readRDS(paste0(largedir,"/MSE_E_50_1_48.rds"))
MSElist_sim[[3]] = readRDS(paste0(largedir,"/MSE_E_50_1_96.rds"))
MSElist_sim[[4]] = readRDS(paste0(largedir,"/MSE_E_50_1_192.rds"))
                                                      
MSElist_sim2=list()
MSElist_sim2[[1]] = readRDS(paste0(largedir,"/MSE_Sm_55_1_16.rds"))
MSElist_sim2[[2]] = readRDS(paste0(largedir,"/MSE_Sm_55_1_48.rds"))
MSElist_sim2[[3]] = readRDS(paste0(largedir,"/MSE_Sm_55_1_96.rds"))
MSElist_sim2[[4]] = readRDS(paste0(largedir,"/MSE_Sm_55_1_192.rds"))


doFig(TCP2(Mlist = MSElist_sim, paste0("nsim = ",c(16,48,96,192)), "Empirical 50 HCR",yzero=T),paste0(figdir,"Fig 9 Emp Simno"))

doFig(TCP2(Mlist = MSElist_sim2, paste0("nsim = ",c(16,48,96,192)), "Sm 55 HCR",yzero=T),paste0(figdir,"Fig 10 Sm Simno"))


MSElist_s48 = MSElist_s96 = MSElist_s192 = list()
MSElist_simcomp[[1]] = readRDS(paste0(largedir,"/MSE_E_50_1_48.rds"))




# Effect of TAC vs F based
MSElist_TF=list()
MSElist_TF[[1]] = readRDS(paste0(largedir,"/MSE_TB_00_1_96.rds"))
MSElist_TF[[2]] = readRDS(paste0(largedir,"/MSE_FB_00_1_96.rds"))
MSElist_TF[[3]] = readRDS(paste0(largedir,"/MSE_FB_01_1_96.rds"))

doFig(TCP2(Mlist = MSElist_TF, paste0("nsim = ",c("TB_00","FB_00","FB_01")), "F vs TAC based",yzero=T),paste0(figdir,"Fig 11 Sm Simno"))




# Effect of OM


# --- Testing ------------------------------------------------------------------

repDataMP = function(x,Data,reps){
  if((max(Data@Year) - Data@LHYear)>5){saveRDS(Data,"C:/temp/JMdata.rds");stop()}
  Rec=new('Rec')
  Rec@Effort = 1
  Rec
}
class(repDataMP) = "MP"

Hist = readRDS(paste0(largedir,"/Hist_1_16.rda"))
test= ProjectMOM(Hist,"repDataMP")
Data = readRDS("C:/temp/JMdata.rds")



# === End of script ===========================================================================



