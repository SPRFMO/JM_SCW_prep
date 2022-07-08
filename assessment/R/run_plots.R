h1nm <- geth(FinModName,"h1")

h1.mod <- readJJM(h1nm, path = "config", input = "input")
h1.diag <- diagnostics(h1.mod,plot=F)