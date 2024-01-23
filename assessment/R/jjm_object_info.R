## head ------------------------------------------------------

library(jjmR)
library(tidyverse)

setwd(file.path(getwd(),"assessment"))
modname <- "1.00"

h2.mod <- runit(geth(modname,"h2"), path="config", input="input")

df <- data.frame(h2.mod[[1]][[5]][[1]]$df)
names(df)<-c("type","year","est","std","lb","ub")
df <- as_tibble(df)

df <- df %>% mutate(
  year=as.numeric(year),
  est=as.numeric(est),
  std=as.numeric(std),
  lb =as.numeric(lb),
  ub =as.numeric(ub)
  )
unique(df$type)
#df %>% filter(type=="Total_Biom") %>%

  df %>%
  ggplot(aes(x=year,y=est,ymin=lb,ymax=ub)) + geom_ribbon(fill="salmon") + expand_limits(y=0) + theme_bw() + ylab("Estimates") +
        facet_wrap(type~.,scales="free")


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


