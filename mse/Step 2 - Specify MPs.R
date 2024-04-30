# ===========================================================================================================
# === Script to make four MPs using Jim's Jack Mackerel Assessment to an OpenMSE operating model ============
# ===========================================================================================================

# Tom Carruthers
# March 2024

# This is designed as a source script

# --- Internal MP functions ----------------------------------------------------

# Index loess smoothing function parameterized as effective number of 
#   parameters (enp) per datum. 

smooth<-function(xx, plot=F, enp.mult=0.25, plotname=""){
  tofill<-!is.na(xx)
  xx[xx==0]<-1E3
  predout<-rep(NA,length(xx))
  dat<-data.frame(x=1:length(xx),y=log(xx))
  enp.target<-sum(tofill)*enp.mult
  out<-loess(y~x,dat=dat,enp.target=enp.target)
  predout[tofill]<-exp(predict(out))
  if(plot){
    plot(xx,type="p",xlab="x",ylab="y",main=plotname)
    lines(predout,col="#ff000090",lwd=2)
  }
  predout
}


# --- Core index ratio management procedure ------------------------------------

IRMP = function(x, DataList, reps=1, enp.mult = 0.25,                          # x is the simulation number
                deltas =  c(NA,  0.529, 2012.446, NA, 2.801, 56.531, 0.419),   # multiplier C = delta * I
                TRP =     c(NA,  182.5, 0.055, NA, 36.77, 1.78, 274.565),      # 'target' index level
                TRPmod = 1.0,                                                  # TRP modifier
                mult = 1.5,                                                    # tuning parameter - multiplier of initial delta
                useInd = c(NA, 1, NA, NA, NA, 1, NA),                          # runs off Peru_CPUE Index only (CV ~34%)
                allo = c(0.0709,  0.6489,  0.2028, 0.0774)){                   # fleet TAC allocation is same as 2023    
  
  Data = DataList[[1]][[1]]                                                    # stock 1, fleet 1 has aggregate data
  thisyr = length(Data@Year)                                                   # index of most recent year in data
  subI = Data@AddInd[x,,]                                                      # take average of last smthyrs years
  smoothI = apply(subI, 1, smooth,enp.mult=enp.mult)                           # smoothed index
  curI = smoothI[nrow(smoothI), ]                                              # get most recent smoothed index
  mod = curI / (TRP*TRPmod); mod[mod>1]=1                                               # linear decline in catch below 'TRP' index level
  TACtot = mean(curI * deltas * useInd * mod, na.rm=T) * mult                  # f
  
  out = list()                                                                 # nested list of management recommendations [[stock]][[fleet]]
  out[[1]] = list()                                                            # only one stock
  for(ff in 1:length(allo)){
    Rec=new('Rec')                                                             # make an object of class Recommendation
    Rec@TAC =  TACtot * allo[ff]                                               # allocated catch
    out[[1]][[ff]] = Rec
  }
  
  out
}

class(IRMP) = "MMP"


# --- Derivatives --------------------------------------------------------------

# cautious > aggressive
IR1A = IR2A = IR3A = IR1B = IR2B = IR3B = IRMP

# Level 1 is MP multiplier
formals(IR1A)$mult = formals(IR1B)$mult = 2.0
formals(IR2A)$mult = formals(IR2B)$mult = 3.0
formals(IR3A)$mult = formals(IR3B)$mult = 4.0

# Level 2 is TRP on / off
formals(IR1A)$TRPmod = formals(IR2A)$TRPmod = formals(IR3A)$TRPmod = 2.0 #twice base TRP (to make some kind of contrast!)
formals(IR1B)$TRP = formals(IR2B)$TRP = formals(IR3B)$TRP = rep(0,7) # no TRP

class(IR1A) =  class(IR2A) = class(IR3A) = class(IR1B) = class(IR2B) = class(IR3B) = "MMP"
IRMPs = c("IR1A","IR1B","IR2A","IR2B","IR3A","IR3B")

cat("Index Ratio MPs Loaded (IR1A, IR1B, IR2A, IR2B - 1/2/3 is multiplier, A/B is TRP/no TRP) \n")
cat("'IRMPs' also specified \n")

# === End of script ===========================================================================



