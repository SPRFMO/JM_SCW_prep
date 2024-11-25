


# BSD = 0.2; BAC = 0.5; Bbias = 1; B1 = 0; B2 = 1; F1 = 0; F2 = 1

Make_shortcut = function(reps=1, BSD = 0.1, BAC = 0.5, Bbias = 1, B1 = 0, B2 = 1, F1 = 0, F2 = 1){
    
  make_MP(Shortcut,HCR_segment,
          B_err = c(BSD, BAC, Bbias),
          OCP_type = "SSB_SSBMSY",
          Ftarget_type = "FMSY",
          OCP = c(B1, B2),
          relF = c(F1, F2))

} 

cat("Shortcut MP code loaded \n")







