## head ------------------------------------------------------

library(jjmR)
library(tidyverse)

setwd(file.path(getwd(),"assessment"))
modname <- "1.00"

h2.mod <- readJJM(geth(modname,"h2"), path="config", input="input")
mod    <- h2.mod
df     <- data.frame(stringsAsFactors = FALSE)

for(main in names(mod[[1]])) {
  for(t in names(mod[[1]][[main]])) {
    if(!is.null(names(mod[[1]][[main]][[1]]))) {
      
      # if substructures exist
      for (s in names(mod[[1]][[main]][[t]])) {
        # print(paste(main, t, s))
        # print(substr(paste(head(mod[[1]][[main]][[t]][[s]], n=1), collapse=" "), 1, 60))
        df <-
          df %>%
          bind_rows(
            data.frame(
              main = main,
              type = t,
              sub  = s,
              class = class(mod[[1]][[main]][[t]][[s]]),
              dim = paste(dim(mod[[1]][[main]][[t]][[s]]), collapse=" / "),
              # dimnames = paste(dimnames(mod[[1]][[main]][[t]][[s]]), collapse=" / "),
              example = substr(paste(head(mod[[1]][[main]][[t]][[s]], n=1), collapse=" "), 1, 60)
            )
          )
       } # end of for loop on s
    
    # if no substructures exist
    } else {
      # print(paste(main, t))
      # print(substr(paste(head(mod[[1]][[main]][[t]][[s]], n=1), collapse=" "), 1, 60))
      df <-
        df %>%
        bind_rows(
          data.frame(
            main = main,
            type = t,
            class = class(mod[[1]][[main]][[t]]),
            dim = paste(dim(mod[[1]][[main]][[t]]), collapse=" / "),
            # dimnames = paste(dimnames(mod[[1]][[main]][[t]]), collapse=" / "),
            example = substr(paste(head(mod[[1]][[main]][[t]], n=1), collapse=" "), 1, 60)
          )
        )
    }# end of is.null (substructures)
  } # end of t
} # end of main

df %>% 
  arrange(main, type, class, sub) %>% 
  pander::pandoc.table(
    style="simple",
    split.tables = 200, 
    justify      = "left"
  )


