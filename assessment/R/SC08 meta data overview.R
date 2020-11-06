# ------------------------------------------------------------------------
# Script for SC08---------------------------------------------------------
# 2020 JM  ---------------------------------------------------------------
# Meta data overview
# ------------------------------------------------------------------------
#install.packages("devtools")
#
# MAKE SURE TO UPDATE
# devtools::install_github("sprfmo/jjmr")
# Remember to recompile jjms as needed

library(jjmR)
library(PBSadmb)
library(tidyverse)
library(ggthemes)

#--------------------------------------------------------
# Working directory should be in assessment folder of jjm
#--------------------------------------------------------

setwd(file.path(getwd(), "assessment"))

pwd <- getwd()
if (!grepl(basename(pwd), "assessment", ignore.case = TRUE)) {
  stop(paste("Set working directory to jjm/assessment"))
}

geth <- function(mod,h=hyp) paste0(h,"_", mod) # Package? Or keep?

FinModName1 <- geth("1.00", h="h1")
modls1 <- readJJM(paste0(FinModName1,".ls"),path="config",input="input")

# generate overviews; first the indices
i <- 
  as.data.frame(modls1[[1]]$data$Inames, stringsAsFactors = FALSE) %>%
  set_names("series") %>% 
  mutate(seriesnr = row_number()) 

df <-
            data.frame(var = "Iyears"   , as_tibble(modls1[[1]]$data$Iyears)   , stringsAsFactors = FALSE) %>% 
  bind_rows(data.frame(var = "Iyearsage", as_tibble(modls1[[1]]$data$Iyearsage), stringsAsFactors = FALSE)) %>% 
  bind_rows(data.frame(var = "Iyearslength", as_tibble(modls1[[1]]$data$Iyearslength), stringsAsFactors = FALSE)) %>% 
  pivot_longer(names_to = "seriesnr", values_to="year", 2:8) %>% 
  mutate(seriesnr = as.integer(gsub("index","", seriesnr)) ) %>% 
  mutate(var     = ifelse(var == "Iyears", "index", var)) %>% 
  mutate(var     = ifelse(var == "Iyearsage", "age", var)) %>% 
  mutate(var     = ifelse(var == "Iyearslength", "length", var)) %>% 
  drop_na() %>% 
  left_join(i, by="seriesnr") %>% 
  mutate(seriestype     = ifelse(grepl("cpue", tolower(series)), "CPUE", "SURVEY")) 
  

# then the catch
i <- 
  as.data.frame(modls1[[1]]$data$Fnames, stringsAsFactors = FALSE) %>%
  set_names("series") %>% 
  mutate(seriesnr = row_number()) 

df <-
            data.frame(var = "Cyearsage"   , as_tibble(modls1[[1]]$data$Fageyears)   , stringsAsFactors = FALSE) %>% 
  bind_rows(data.frame(var = "Cyearslength", as_tibble(modls1[[1]]$data$Flengthyears), stringsAsFactors = FALSE)) %>% 
  pivot_longer(names_to = "seriesnr", values_to="year", 2:5) %>% 
  mutate(seriesnr = as.integer(gsub("fishery","", seriesnr)) ) %>% 
  mutate(var     = ifelse(var == "Cyearsage", "age", var)) %>% 
  mutate(var     = ifelse(var == "Cyearslength", "length", var)) %>% 
  drop_na() %>% 
  left_join(i, by="seriesnr") %>% 
  mutate(seriestype = "CATCH") %>% 
  bind_rows(df)


df %>% 
  mutate(series2 = paste(seriesnr, series,var,sep=" / ")) %>% 
  ggplot(aes(x=year, y=reorder(series2, desc(series2)))) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_point(aes(colour=series), size=4) +
  labs(x="", y="") +
  facet_grid(seriestype~., scales="free_y", space="free")




