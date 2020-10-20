# ------------------------------------------------------------------------
# Script for SC08---------------------------------------------------------
# 2020 JM  ---------------------------------------------------------------
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


#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# summarize H1 and H2
#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# FinMod <- mod1.00

# FinModName <- "mod1.00"
FinModName1 <- geth("1.00", h="h1")
FinModName2 <- geth("1.00", h="h2")

modhl1 <- readJJM(paste0(FinModName1,".hl"),path="config",input="input")
modll1 <- readJJM(paste0(FinModName1,".ll"),path="config",input="input")
modhs1 <- readJJM(paste0(FinModName1,".hs"),path="config",input="input")
modls1 <- readJJM(paste0(FinModName1,".ls"),path="config",input="input")

modhl2 <- readJJM(paste0(FinModName2,".hl"),path="config",input="input")
modll2 <- readJJM(paste0(FinModName2,".ll"),path="config",input="input")
modhs2 <- readJJM(paste0(FinModName2,".hs"),path="config",input="input")
modls2 <- readJJM(paste0(FinModName2,".ls"),path="config",input="input")

lastF <-  modls1[[1]]$output[[1]]$f
tail(modls1[[1]]$output[[1]]$msy_mt[,c(6)],1)


mdf <- 
            data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="SSB", modls1[[1]]$output[[1]]$SSB) %>% 
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south",Var="SSB", modls2[[1]]$output[[1]]$SSB)) %>% 
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north",Var="SSB", modls2[[1]]$output[[2]]$SSB)) %>% 
  
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Recruitment", modls1[[1]]$output[[1]]$R)) %>% 
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south",Var="Recruitment", modls2[[1]]$output[[1]]$R)) %>% 
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north",Var="Recruitment", modls2[[1]]$output[[2]]$R)) %>% 

  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Fishing mortality", modls1[[1]]$output[[1]]$msy_mt[,c(1,6)])) %>% 
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south",Var="Fishing mortality", modls2[[1]]$output[[1]]$msy_mt[,c(1,6)])) %>% 
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north",Var="Fishing mortality", modls2[[1]]$output[[2]]$msy_mt[,c(1,6)])) %>% 
  
  setNames( c("Hyp", "Scenario","Stock","Var", "Year","Value","SD","lb","ub")) %>% 
  mutate(Var = factor(Var, levels=c("Catch", "SSB","Fishing mortality","Recruitment"))) %>% 
  mutate(Stock = tolower(Stock)) %>% 
  mutate(Hyp = ifelse(Hyp=="H1", "H1 (one stock)", "H2 (two stock)"))  %>% 
  mutate(Stock = ifelse(Stock=="comb","", Stock)) 

fc <- 
            data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="SSB", Fmult="1.0", modls1[[1]]$output[[1]]$SSB_fut_1)%>% 
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="SSB", Fmult="0.75", modls1[[1]]$output[[1]]$SSB_fut_2))%>% 
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="SSB", Fmult="1.25", modls1[[1]]$output[[1]]$SSB_fut_3))%>% 
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="SSB", Fmult="FMSY", modls1[[1]]$output[[1]]$SSB_fut_4))%>% 
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="SSB", Fmult="0.0", modls1[[1]]$output[[1]]$SSB_fut_5))%>% 
  
  # Need to put the Fs into a forecast string with year and value
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Fishing mortality", Fmult="1.0", 1.00*tail(modls1[[1]]$output[[1]]$msy_mt[,c(6)],1) ))%>% 
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Fishing mortality", Fmult="0.75",0.75*tail(modls1[[1]]$output[[1]]$msy_mt[,c(6)],1) ))%>% 
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Fishing mortality", Fmult="1.25",1.25*tail(modls1[[1]]$output[[1]]$msy_mt[,c(6)],1) ))%>% 
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Fishing mortality", Fmult="FMSY", NA))%>% 
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Fishing mortality", Fmult="0.0", 0.00 ))%>% 
  
  setNames( c("Hyp", "Scenario","Stock","Var", "Fmult", "Year","Value","SD","lb","ub")) %>% 
  mutate(Var = factor(Var, levels=c("Catch", "SSB","Fishing mortality","Recruitment"))) %>% 
  mutate(Hyp = ifelse(Hyp=="H1", "H1 (one stock)", "H2 (two stock)"))  %>% 
  mutate(Stock = ifelse(Stock=="comb","", Stock)) 

cc1 <- 
            data.frame(Hyp="H1", Model="LS, SS", Var="Catch", rownames_to_column(as.data.frame(modls1[[1]]$data$Fcaton), var="Year") ) %>% 
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", Var="Catch", rownames_to_column(as.data.frame(modls2[[1]]$data$Fcaton), var="Year")) ) %>% 
  
  pivot_longer(names_to="Stock", values_to="Value", fishery1:fishery4) %>% 
  mutate(Stock = ifelse(Hyp=="H1", "comb", Stock)) %>% 
  mutate(Stock = ifelse(Hyp=="H2" & Stock == "fishery3", "north", Stock)) %>% 
  mutate(Stock = ifelse(Hyp=="H2" & Stock %in% c("fishery1","fishery2","fishery4"), "south", Stock)) %>% 
  group_by(Hyp, Model, Stock, Var, Year) %>% 
  summarise(Value = sum(Value, na.rm=TRUE)) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(Var = factor(Var, levels=c("Catch","SSB","Fishing mortality","Recruitment")))  %>% 
  mutate(Hyp = ifelse(Hyp=="H1", "H1 (one stock)", "H2 (two stock)"))  %>% 
  mutate(Stock = ifelse(Stock=="comb","", Stock)) 
  

cc2 <-
            data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Catch", Fmult="1.0", modls1[[1]]$output[[1]]$Catch_fut_1) %>% 
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Catch", Fmult="0.75", modls1[[1]]$output[[1]]$Catch_fut_2)) %>% 
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Catch", Fmult="1.25", modls1[[1]]$output[[1]]$Catch_fut_3))%>% 
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Catch", Fmult="FMSY", modls1[[1]]$output[[1]]$Catch_fut_4))%>% 
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Catch", Fmult="0.0", modls1[[1]]$output[[1]]$Catch_fut_5))%>% 
  
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south" ,Var="Catch", Fmult="1.0", modls2[[1]]$output[[1]]$Catch_fut_1)) %>% 
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south" ,Var="Catch", Fmult="0.75", modls2[[1]]$output[[1]]$Catch_fut_2)) %>% 
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south" ,Var="Catch", Fmult="1.25", modls2[[1]]$output[[1]]$Catch_fut_3))%>% 
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south" ,Var="Catch", Fmult="FMSY", modls2[[1]]$output[[1]]$Catch_fut_4))%>% 
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south" ,Var="Catch", Fmult="0.0", modls2[[1]]$output[[1]]$Catch_fut_5))%>% 

  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north" ,Var="Catch", Fmult="1.0", modls2[[1]]$output[[2]]$Catch_fut_1)) %>% 
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north" ,Var="Catch", Fmult="0.75", modls2[[1]]$output[[2]]$Catch_fut_2)) %>% 
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north" ,Var="Catch", Fmult="1.25", modls2[[1]]$output[[2]]$Catch_fut_3))%>% 
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north" ,Var="Catch", Fmult="FMSY", modls2[[1]]$output[[2]]$Catch_fut_4))%>% 
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north" ,Var="Catch", Fmult="0.0", modls2[[1]]$output[[2]]$Catch_fut_5))%>% 
  
  
  setNames( c("Hyp", "Scenario","Stock","Var", "Fmult", "Year","Value")) %>% 
  mutate(Year = as.numeric(Year)) %>% 
  mutate(Var = factor(Var, levels=c("Catch","SSB","Fishing mortality","Recruitment")))  %>% 
  mutate(Hyp = ifelse(Hyp=="H1", "H1 (one stock)", "H2 (two stock)"))  %>% 
  mutate(Stock = ifelse(Stock=="comb","", Stock)) %>% 
  mutate(fcgroup = paste0(Fmult, Stock)) 
  



# plot nominal
ep <-
  mdf %>% 
  filter(Year == 2020) 

mdf %>% 
  filter(Year >= 1974) %>% 
  # View()

  ggplot(aes(x=Year, y=Value, group=Stock)) +
  theme_bw() +
  theme(legend.position = "none") +
  
  geom_line(aes(colour=Stock)) +
  geom_ribbon(aes(fill=Stock, ymin=lb, ymax=ub), alpha=0.4) +
  ggrepel::geom_text_repel(data=ep,
                           aes(x=Year, y=Value, label = Stock, colour=Stock)) +
  
  geom_line(data=filter(cc1, Year >= 1974),
            aes(x=Year, y=Value, colour=Stock), inherit.aes = FALSE) +
  geom_line(data=cc2, aes(x=Year, y=Value, colour=Stock, linetype=Fmult, group=fcgroup), inherit.aes = FALSE) +
  geom_line(data=fc, aes(x=Year, y=Value, colour=Stock, linetype=Fmult, group=Fmult), inherit.aes = FALSE) +
  
  labs(y="", x="") +
  
  # ggsci::scale_colour_jco() +
  # ggsci::scale_fill_jco() +

  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +
  
  facet_grid(Var ~ Hyp, scales="free_y")
