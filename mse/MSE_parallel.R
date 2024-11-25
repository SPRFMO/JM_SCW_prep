# For running openMSE in parallel
join_arrays = MSEtool:::join_arrays
join_list_of_arrays = MSEtool:::join_list_of_arrays

mergeMPs = function(MSEobjs){
  if (!inherits(MSEobjs, "list")) 
    stop("MSEobjs must be a list")
  if (length(MSEobjs) < 2) 
    stop("MSEobjs list doesn't contain multiple MSE objects")
  if (!all(sapply(MSEobjs, inherits, "MSE"))) 
    stop("MSEobjs must be a list of objects of class `MSE`", 
         call. = FALSE)
  
  slots_identical <- function(slotname, MSEobjs, is_logical = FALSE) {
    templist <- lapply(MSEobjs, slot, slotname)
    is_identical <- vapply(templist[-1], identical, logical(1), 
                           templist[[1]]) %>% all()
    if (is_logical) {
      return(is_identical)
    }
    else {
      return(templist %>% unlist() %>% unique())
    }
  }
  if (!slots_identical("nsim", MSEobjs, TRUE)) 
    stop("nsim slot not identical in all MSE objects.")
  if (!slots_identical("nyears", MSEobjs, TRUE)) 
    stop("nyears slot not identical in all MSE objects.")
  if (!slots_identical("proyears", MSEobjs, TRUE)) 
    stop("proyears slot not identical in all MSE objects.")
  MSE <- new("MSE", Name = slots_identical("Name", 
                                           MSEobjs), nyears = slots_identical("nyears", MSEobjs), 
             proyears = slots_identical("proyears", MSEobjs), 
             nMPs = sapply(MSEobjs, slot, "nMPs") %>% sum(), 
             MPs = lapply(MSEobjs, slot, "MPs") %>% unlist(), 
             nsim = slots_identical("nsim", MSEobjs), OM = MSEobjs[[1]]@OM, 
             Obs = MSEobjs[[1]]@Obs, SB_SBMSY = join_arrays(MSEobjs, "SB_SBMSY", along = 2), 
                                     F_FMSY = join_arrays(MSEobjs, "F_FMSY", along = 2), 
                                     N = join_arrays(MSEobjs,"N", along = 3), 
                                     B = join_arrays(MSEobjs, "B",  along = 2), 
                                     SSB = join_arrays(MSEobjs, "SSB", along = 2), 
                                     VB = join_arrays(MSEobjs, "VB", along = 2), 
                                     FM = join_arrays(MSEobjs, "FM", along = 2), SPR = lapply(MSEobjs, slot, "SPR") %>% 
               
               join_list_of_arrays(along = 2), Catch = join_arrays(MSEobjs,  "Catch", along = 2), 
             Removals = join_arrays(MSEobjs,  "Removals", along = 2), 
             Effort = join_arrays(MSEobjs,  "Effort", along = 2), TAC = join_arrays(MSEobjs, "TAC", along = 2), 
             TAE = join_arrays(MSEobjs, "TAE", along = 2), 
             BioEco = lapply(MSEobjs, slot, "BioEco") %>% join_list_of_arrays(along = 2), 
             RefPoint = list(), CB_hist = MSEobjs[[1]]@CB_hist, FM_hist = MSEobjs[[1]]@FM_hist, 
             SSB_hist = MSEobjs[[1]]@SSB_hist, Hist = MSEobjs[[1]]@Hist, 
             PPD = lapply(MSEobjs, slot, "PPD") %>% unlist(), 
             Misc = list())
  MSE@RefPoint <- local({
    Refout <- lapply(MSEobjs, function(x) x@RefPoint[c("MSY", 
                                                       "FMSY", "SSBMSY", "F_SPR")]) %>% 
      join_list_of_arrays(along = 2)
    Refout$Dynamic_Unfished <- MSEobjs[[1]]@RefPoint$Dynamic_Unfished
    Refout$ByYear <- MSEobjs[[1]]@RefPoint$ByYear
    Refout
  })
  if (length(MSEobjs[[1]]@Misc$extended)) {
    MSE@Misc$extended <- lapply(MSEobjs, function(x) x@Misc$extended) %>% 
      join_list_of_arrays(along = 3)
  }
  attr(MSE, "version") <- packageVersion("MSEtool")
  attr(MSE, "date") <- date()
  attr(MSE, "R.version") <- R.version
  return(MSE)
}


Project_parallel = function(Hist,MPs){
  
  if(class(Hist)!='Hist')stop("First argument must be a historical operating model object made by runMSE(..., Hist=T)")
  
  nMP = length(MPs)
  nproc = parallel::detectCores()/2 # ignore hyperthreading
  sfInit(parallel = T, cpus=min(nMP, nproc))
  sfLibrary(MSEtool)
  sfLibrary(SAMtool)
  #sfLibrary(spict)
  #sfExport('fit.spict.robust')
  sfExportAll()
  
  sfLapply(1:nMP,function(x,Hist,MPs)Project(Hist,MPs=MPs[x],checkMPs=F), MPs = MPs, Hist=Hist)

}


cat("Parallel MSE running code loaded \n")

