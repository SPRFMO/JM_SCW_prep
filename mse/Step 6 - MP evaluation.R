# ===========================================================================================================
# === Script to run the specified MPs against all OMs =======================================================
# ===========================================================================================================

# Tom Carruthers
# March 2024

# This is designed as a source script

# --- Prerequisites ------------------------------------------------------------

setwd("C:/GitHub/jjm/assessment") # Tom WD

library(openMSE)
source("../mse/Step 2 - Specify MPs.r")   # load MPs made in step 2
source("../mse/Source_JM2OM.r")           # load JM2OM source code
getOMs(OMdir = "../mse/OMs/")                     # load OMs  (OMnam, nOM, runnam)  
largedir = "C:/temp/JM_mcmc/"             # somewhere to store big things off the github 
                                          #  (needs /Hists and /MSEs subdirectories)


# --- Run MSE spool up ---------------------------------------------------------

# This just means that you don't have to do all the MSY calcs etc each time
# you want to project some new MPs


setwd("C:/GitHub/jjm/assessment") # Tom WD
#source("../mse/Step 2 - Specify MPs.r")   # load MPs made in step 2
source("../mse/Source_JM2OM.r")           # load JM2OM source code
source("../mse/MSE_parallel.r")           # load parallel coade
source("../mse/MP_spawn.r")               # load MP building

largedir = "C:/Users/tcar_/Dropbox/temp/JJM_MPtest"

library(tidyverse)
library(flextable)
library(libr)
library(fmtr)
library(abind)
library(data.table)
library(openMSE)


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
sims = c(16, 48, 96)
for(ss in 1:length(sims)){
  for(i in 1:4){
    OM = GetOM_Jim(paste0(largedir,"/mcmc/om",i,"/mcmc"), hyp[i],nsim=sims[ss])
    Hist = runMSE(IFR(OM), Hist = T)
    saveRDS(Hist, paste0(largedir,"/Hist_",i,"_",sims[ss],".rda"))
  }
}

MSElist = list()
for(i in 1:4){
  Hist = readRDS(paste0(largedir,"/Hist_",i,"_16.rda"))
  MSElist[[i]] = Project(Hist,"curE")
}

par(mfrow=c(2,2))
for(i in 1:length(MSElist))matplot(t(MSElist[[i]]@SB_SBMSY[,1,]),type="l",main = i)



# --- Make MPs -----------------------------------------------------------------

source.all('../mse/MPs/')
ntune = 8
Fseq = seq(0.5,1.2,length.out=ntune)
Flab = Fseq*10

make_s_list = function(B1=0 , B2=1 , F1=1, F2=1, Fseq = seq(0.5,1.2,length.out=8),
                       BSD = 0.15, BAC = 0, Bbias = 1 ){
  ntune = length(Fseq)
  list(B1 = rep(B1,ntune), B2 = rep(B2,ntune), F1 = F1*Fseq, F2 = F2*Fseq, 
       BSD = rep(BSD,ntune), BAC = rep(BAC,ntune), Bbias = rep(Bbias,ntune))
} 

# --- Short cut good BSD = 0.15, BAC = 0, Bbias = 1
Sg_01 = paste0("Sg_01_",Flab); spawnSC(make_s_list(), Sg_01)                 # constant F (HCR 0,1  1,1 )
Sg_05 = paste0("Sg_05_",Flab); spawnSC(make_s_list(F1=0.5), Sg_05)           # HCR 0,1  0.5,1 
Sg_00 = paste0("Sg_00_",Flab); spawnSC(make_s_list(F1=0), Sg_00 )            # HCR 0,1  0,1 
Sg_55 = paste0("Sg_55_",Flab); spawnSC(make_s_list(F1=0.5, B1 = 0.5), Sg_55) # HCR 0.5,1  0.5,1 
Sg_50 = paste0("Sg_50_",Flab); spawnSC(make_s_list(F1=0, B1 = 0.5), Sg_50)   # HCR 0.5,1  0.5,1 

#  --- Short cut moderate BSD = 0.2, BAC = 0.6, Bbias = 1
Sm_01 = paste0("Sm_01_",Flab); spawnSC(make_s_list(BSD=0.2, BAC=0.6), Sm_01)                   # constant F (HCR 0,1  1,1 )
Sm_05 = paste0("Sm_05_",Flab); spawnSC(make_s_list(BSD=0.2, BAC=0.6, F1=0.5), Sm_05)           # HCR 0,1  0.5,1 
Sm_00 = paste0("Sm_00_",Flab); spawnSC(make_s_list(BSD=0.2, BAC=0.6, F1=0), Sm_00)             # HCR 0,1  0,1 
Sm_55 = paste0("Sm_55_",Flab); spawnSC(make_s_list(BSD=0.2, BAC=0.6, F1=0.5, B1 = 0.5), Sm_55) # HCR 0.5,1  0.5,1 
Sm_50 = paste0("Sm_50_",Flab); spawnSC(make_s_list(BSD=0.2, BAC=0.6, F1=0, B1 = 0.5), Sm_50)   # HCR 0.5,1  0.5,1 

# --- Short cut bad BSD = 0.25, BAC = 0.75, Bbias = 1.25
Sb_01 = paste0("Sb_01_",Flab); spawnSC(make_s_list(BSD=0.25, BAC=0.75, Bbias=1.25), Sb_01)                   # constant F (HCR 0,1  1,1 )
Sb_05 = paste0("Sb_05_",Flab); spawnSC(make_s_list(BSD=0.25, BAC=0.75, Bbias=1.25, F1=0.5), Sb_05)           # HCR 0,1  0.5,1 
Sb_00 = paste0("Sb_00_",Flab); spawnSC(make_s_list(BSD=0.25, BAC=0.75, Bbias=1.25, F1=0), Sb_00)             # HCR 0,1  0,1 
Sb_55 = paste0("Sb_55_",Flab); spawnSC(make_s_list(BSD=0.25, BAC=0.75, Bbias=1.25, F1=0.5, B1 = 0.5), Sb_55) # HCR 0.5,1  0.5,1 
Sb_50 = paste0("Sb_50_",Flab); spawnSC(make_s_list(BSD=0.25, BAC=0.75, Bbias=1.25, F1=0, B1 = 0.5), Sb_50)   # HCR 0.5,1  0.5,1 


# --- Assessment via rapid SCA 
make_a_list = function(B1=0 , B2=1 , F1=1, F2=1, Fseq = seq(0.5,1.2,length.out=8)){
  ntune = length(Fseq)
  list(B1 = rep(B1,ntune), B2 = rep(B2,ntune), F1 = F1*Fseq, F2 = F2*Fseq)
} 

A_01 = paste0("A_01_",Flab); spawnSC(make_a_list(), A_01)                 # constant F (HCR 0,1  1,1 )
A_05 = paste0("A_05_",Flab); spawnSC(make_a_list(F1=0.5), A_05)           # HCR 0,1  0.5,1 
A_00 = paste0("A_00_",Flab); spawnSC(make_a_list(F1=0), A_00 )            # HCR 0,1  0,1 
A_55 = paste0("A_55_",Flab); spawnSC(make_a_list(F1=0.5, B1 = 0.5), A_55) # HCR 0.5,1  0.5,1 
A_50 = paste0("A_50_",Flab); spawnSC(make_a_list(F1=0, B1 = 0.5), A_50)   # HCR 0.5,1  0.5,1 
  

# --- Empirical 

Ifacs = seq(0.8, 1.5, length.out=8)
Ilab = Ifacs*10
spawnlist = list(Ind_fac = Ifacs)

E_01 = paste0("E_01_",Ilab); spawnMP('Emp', defaults=list(HCR_CP_B = c(0,1),   HCR_CP_TAC=c(1,1)),   spawnlist, E_01) # constant F (HCR 0,1  1,1 )
E_05 = paste0("E_05_",Ilab); spawnMP('Emp', defaults=list(HCR_CP_B = c(0,1),   HCR_CP_TAC=c(0.5,1)), spawnlist, E_05) # HCR 0,1  0.5,1 
E_00 = paste0("E_00_",Ilab); spawnMP('Emp', defaults=list(HCR_CP_B = c(0,1),   HCR_CP_TAC=c(0,1)),   spawnlist, E_00) # HCR 0,1  0,1 
E_55 = paste0("E_55_",Ilab); spawnMP('Emp', defaults=list(HCR_CP_B = c(0.5,1), HCR_CP_TAC=c(0.5,1)), spawnlist, E_55) # HCR 0.5,1  0.5,1 
E_50 = paste0("E_50_",Ilab); spawnMP('Emp', defaults=list(HCR_CP_B = c(0.5,1), HCR_CP_TAC=c(0,1)),   spawnlist, E_50) # HCR 0.5,1  0.5,1 
  

# --- 16 sim test --------------------------------------------------------------

OMnams = c("Hist_1_16.rda", "Hist_3_16.rda")

repDataMP = function(x,Data,reps){
  if((max(Data@Year) - Data@LHYear)>5){saveRDS(Data,"C:/temp/JMdata.rds");stop()}
  Rec=new('Rec')
  Rec@Effort = 1
  Rec
}
class(repDataMP) = "MP"

test = Project(readRDS(paste0(largedir,"/", OMnams[1])), c("Sg_01_5","A_01_5","E_01_12"))


 Data = readRDS("C:/temp/JMdata.rds")


MPsets = paste0(rep(c("Sg","Sm","Sb","A","E"),each=5),"_",c("01","05","00","55","50"))
MPind = grep("01",MPsets)
MPind = 1:length(MPsets)
setup()

om = 1
Hist = readRDS(paste0(largedir,"/", OMnams[om]))

for(i in MPind){
  MSEnam = paste0("MSE_",MPsets[i],"_",om)
  temp = Project_parallel(Hist, get(MPsets[i]))
  assign(MSEnam, mergeMPs(temp))
  saveRDS(get(MSEnam), paste0(largedir,"/",MSEnam,".rds"))
}  







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


# === 


# !!!! remember to set up allocation here MOM@Allocate

for(om in 1:nOM){
  Hist = multiMSE(IFR(get(OMnam[om])), Hist = T) # !!!! IFR is Inflate Future Recruitment - a temporary patch for demo purposes !!!
  saveRDS(Hist,paste0(largedir,"Hists/Hist_",runnam[om],".rda"))
} 


# --- Run MSE projections ------------------------------------------------------

for(om in 1:nOM){
  Hist = readRDS(paste0(largedir,"Hists/Hist_",runnam[om],".rda"))
  MSE = ProjectMOM(Hist, IRMPs)
  saveRDS(MSE,paste0(largedir,"MSEs/MSE_",runnam[om],".rda"))
} 




# === End of script ===========================================================================



