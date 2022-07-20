# devtools::install_github("danovando/jjmR")
library(jjmR)
library(tidyverse)
library(kableExtra)
library(ggthemes)
library(patchwork)

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
plot_comp <- function(tidymods=tidy_mods,thismet="R") {
  tidymods$totals |> filter(metric==thismet) |> 
    ggplot(aes(x=year, y=value, ymin = lowerbound, ymax = upperbound, fill = model) ) + 
    geom_ribbon(alpha = 0.2)  + geom_line(aes(color=model),alpha=.9,size=.5) + theme_few(base_size=16) +
    ylab(thismet)
}
#--------------------------------------------
FinModName <- "1.13"
getwd()
modnm <- "1.13"
mod.list <- c("h1_0.00","h1_1.00")
mod.list
  m00 <- readJJM("h1_0.00", path = "config", input = "input")  
  m03 <- readJJM("h1_0.03", path = "config", input = "input")  
  m04 <- readJJM("h1_0.04", path = "config", input = "input")  
  m100 <- readJJM("h1_1.00", path = "config", input = "input")  
  m109 <- readJJM("h1_1.00", path = "config", input = "input")  
  m109 <- runit("h1_1.09",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
  m113 <- readJJM("h1_1.13", path = "config", input = "input")  
  h2m00 <- readJJM("h2_0.00", path = "config", input = "input")  
  h2m13 <- readJJM("h2_1.13", path = "config", input = "input")  
  
  kobe(combineModels(h2m00,h2m13) |> filter(k), engine = "ggplot") + facet_grid(model~stock,scales='free')
 p1<- kobe(h2m00, engine = "ggplot") + facet_grid(model~stock,scales='free') 
  p2<- kobe(h2m13, engine = "ggplot") + facet_grid(model~stock,scales='free')
  p1/p2
  
 str(combineModels(h2m00,h2m13)) #|> filter(stock==2)# , engine = "ggplot") + facet_grid(model~.,scales='free')
  kobe(h2m00, engine = "ggplot") + facet_grid(model~stock,scales='free')

glimpse(tidy_mods$totals |> filter(metric=="SSB"))
names(tidy_mods$fishing_mortality)
unique(tidy_mods$totals$metric)
thismet="SSB","R"
thismet="R"
r<-tidy_mods$recruits |>  ggplot() + 
  geom_ribbon(aes(year, ymin = lower_recruits, ymax = upper_recruits, fill = model),alpha = 0.5) + 
  geom_line(aes(year, recruits, color = model)) + theme_few(base_size=16);r

tidy_mods <- tidy_JJM(combineModels(m00,m03,m04))
tidy_mods <- tidy_JJM(combineModels(m100,m109,m113))
tidy_mods <- tidy_JJM(combineModels(m00,m113))
b=plot_comp(tidymods=tidy_mods,thismet="SSB")
r=plot_comp(tidymods=tidy_mods,thismet="R")
r/b

#---------------------------
tidy_JJM(m109)$index_fits %>% 
  ggplot() + 
  geom_pointrange(aes(year, observed_ind, ymin = observed_ind - 1.96 * observed_se, ymax =  observed_ind + 1.96 * observed_se), alpha = 0.5) +
  geom_path(aes(year, pred_ind, color = model)) + 
  facet_wrap(~ fleet_name, scales = "free_y") + 
  scale_x_continuous(name = "Year", guide = guide_axis(n.dodge = 2)) + 
  scale_y_continuous(name = "Index Values")
totals <- get_totals(tidy_mods)

totals %>% 
  ggplot(aes(year, value, color = stock, linetype = model)) + 
  geom_line() + 
  facet_wrap(~ metric, scales = "free_y")

# note 1.00 is same as 0.04
m1 <- runit("h1_1.00",pdf=TRUE,portrait=F,est=TRUE,exec="../src/jjms")
combineModels(m0,m1)

mod.list <- 
  list.files(path="results", pattern=".pdf") %>% 
  grep(pattern='Retro', inv=T, value=T) %>% 
  grep(pattern='h1_1.02|h2_0.02', inv=T, value=T) %>% 
  gsub("\\.pdf","", .)

for (i in mod.list) {
  print(i)
  assign(i,
         readJJM(i, path = "config", input = "input"))  
}

# compare 0.00 and 0.04
mods          <- jjmR::combineModels(h1_0.00, h1_0.04)

selectivities <- jjmR::get_selectivities(mods)
msy           <- jjmR::get_msy_mt(old_vs_new_mods)
diag          <- jjmR::diagnostics(mods)

plot_selectivities(selectivities, years=2000:2020)

jjmR::diagnostics(mods)[[1]][["Stock_1"]][["data"]][["ageFleetsPlots"]]
jjmR::diagnostics(mods)[[1]]$Stock_1$data$WeightAge
diag[[1]]$Stock_1$data$weightAge
diag[[1]]$Stock_1$data$weightFishery



