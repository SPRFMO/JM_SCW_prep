library(jjmR)
library(tidyverse)
library(kableExtra)

# pwd <- getwd()
# if (!grepl(basename(pwd), "assessment", ignore.case = TRUE)) {
#   stop(paste("Set working directory to jjm/assessment"))
# }

geth <- function(mod,h=hyp) paste0(h,"_", mod)

fn.plotind <- function(mods2compare, indname) {
  fn.seldata <- function(x) {
    x$data$Index[,i] %>%
      bind_rows() %>%
      pivot_longer(everything(), names_to="year") %>%
      drop_na() %>%
      mutate(year=as.numeric(year),
             assessment_year=max(year))
  }
  
  mods <- compareModels(geth(mods2compare, "h2"))
  i <- grep(indname,mods[[1]]$data$Inames)
  dat2use <- list()
  for(m in 1:length(mods)) {
    dat2use[[m]] <- fn.seldata(mods[[m]])
  }
  
  p <- map_dfr(dat2use, ~as_tibble(.)) %>%
    mutate(assessment_year=as.factor(assessment_year)) %>%
    ggplot(aes(x=year,y=value,colour=assessment_year)) +
    geom_line() +
    theme_minimal() + 
    scale_x_continuous(breaks= scales::pretty_breaks())
  print(p)
}

fixed_bmsy <- function(mod,refpt=5500){
  old_rat <- (mod[[1]]$output[[1]]$msy_mt[,13])
  new_rat <- (mod[[1]]$output[[1]]$msy_mt[,12]/ refpt)
  mod[[1]]$output[[1]]$msy_mt[,13] <- new_rat
  mod[[1]]$output[[1]]$msy_mt[,10] <- refpt
  return(mod)
}

FinModName <- "1.05"

setwd("assessment")
getwd()

modnm <- "1.03"

tmp1 <- runit(geth(modnm,"h1"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")

tmp2 <- runit(geth(modnm,"h2"),pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
