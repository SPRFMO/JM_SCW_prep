## head ------------------------------------------------------

library(jjmR)
library(tidyverse)
library(magrittr)
library(reshape2)
library(ggthemes)

# setwd(getwd())

construct_input_tables<- function(modname, docname="Input_Tables") {
docname <- paste0(docname,".doc")

h1nm <- geth(modname,"h1")
h2nm <- geth(modname,"h2")

h1_mod <- readJJM(h1nm, path = "../../assessment/config", input = "../../assessment/input")
h2_mod <- readJJM(h2nm, path = "../../assessment/config", input = "../../assessment/input")

path.m = eval(parse(text=paste0("h1_mod$",names(h1_mod))))

Table_1 <- path.m$data$Fcaton %>% data.frame() %>% rownames_to_column(var = 'Year') %>%
  set_colnames(c('Year','Fleet 1','Fleet 2','Fleet 3','Fleet 4')) %>% mutate(Year = as.numeric(Year))

Table_2 <- path.m$output$Stock_1$pobs_fsh_1 %>% data.frame() %>% set_colnames(c('Year', seq(1:12))) 
Table_2[,-1] <- Table_2[,-1]*100

Table_3 <- path.m$output$Stock_1$pobs_fsh_2 %>% data.frame() %>% set_colnames(c('Year', seq(1:12))) 
Table_3[,-1] <- Table_3[,-1]*100

Table_4 <- path.m$output$Stock_1$pobs_fsh_4 %>% data.frame() %>% set_colnames(c('Year', seq(1:12))) 
Table_4[,-1] <- Table_4[,-1]*100

Table_5 <- path.m$output$Stock_1$pobs_len_fsh_3 %>% data.frame() %>% set_colnames(c('Year', path.m$data$lengthbin)) 
Table_5[,-1] <- Table_5[,-1]*100

naInd = path.m$output[[1]]$Index_names
nj    = NULL
parnames <- paste0("Obs_Survey_",1:length(naInd))
for(i in 1:length(naInd)){
  tmp.nj <- path.m$output[[1]][parnames[i]] %>% data.frame() %>% 
    set_colnames(c('Year','Obs','Pred','se','pr','lpr')) %>% full_join(data.frame(Year=path.m$data$years[1]:path.m$data$years[2])) %>% 
    select(Year, Obs) %>% arrange(Year)
  if (i == 1 ){
    nj <- bind_cols(nj,tmp.nj)
  } else {
    nj <- cbind(nj,tmp.nj$Obs)
  }
}


Table_6 <- nj %>% data.frame() %>% set_colnames(c('Year','Chile (1)','Chile (2)','Chile (3)','Chile (4)','Peru(2)',
                                                  'Peru(3)','Offshore') )

Table_7 <- path.m$output$Stock_1$pobs_ind_1 %>% data.frame() %>% set_colnames(c('Year', seq(1:12))) 
Table_7[,-1] <- Table_7[,-1]*100

Table_8 <- path.m$output$Stock_1$pobs_ind_2 %>% data.frame() %>% set_colnames(c('Year', seq(1:12))) 
Table_8[,-1] <- Table_8[,-1]*100

Table_9 <- path.m$output$Stock_1$pobs_ind_4 %>% data.frame() %>% set_colnames(c('Year', seq(1:12))) 
Table_9[,-1] <- Table_9[,-1]*100

Table_10 <- path.m$output$Stock_1$wt_fsh_1 %>% data.frame() %>% set_colnames(c('Year', seq(1,12))) 

Table_11 <- path.m$output$Stock_1$wt_fsh_2 %>% data.frame() %>% set_colnames(c('Year', seq(1,12))) 

Table_12 <- path.m$output$Stock_1$wt_fsh_3 %>% data.frame() %>% set_colnames(c('Year', seq(1,12))) 

Table_13 <- path.m$output$Stock_1$wt_fsh_4 %>% data.frame() %>% set_colnames(c('Year', seq(1,12))) 

sc_years <- tibble(Assessmentyear=2013:as.numeric(format(Sys.time(), "%Y")), SC=paste0("SC",1:(as.numeric(format(Sys.time(), "%Y"))-2013+1)))
Table_27 <- read_csv("../../assessment/hist_retro/SPRFMO historical retro.csv") %>% filter(assessmenttype %in% c("assessment","update")) %>% select(Assessmentyear, Year, SSB) %>% left_join(sc_years) %>% select(-Assessmentyear) %>% pivot_wider(names_from=SC, values_from=SSB)

# Estimated begin-year numbers at age
Table_28 <- h1_mod[[1]]$output$Stock_1$N %>% data.frame() %>% set_colnames(c('Year', seq(1,12))) 
Table_29 <- h2_mod[[1]]$output$Stock_1$N %>% data.frame() %>% set_colnames(c('Year', seq(1,12))) 
Table_30 <- h2_mod[[1]]$output$Stock_2$N %>% data.frame() %>% set_colnames(c('Year', seq(1,12))) 

# Estimated total fishing mortality at age
Table_31 <- cbind(h1_mod[[1]]$output$Stock_1$N[,1],h1_mod[[1]]$output$Stock_1$TotF) %>% data.frame() %>% set_colnames(c('Year', seq(1,12))) 
Table_32 <- cbind(h2_mod[[1]]$output$Stock_1$N[,1],signif(h2_mod[[1]]$output$Stock_1$TotF,3)) %>% data.frame() %>% set_colnames(c('Year', seq(1,12))) 
Table_33 <- cbind(h2_mod[[1]]$output$Stock_2$N[,1],signif(h2_mod[[1]]$output$Stock_2$TotF,3)) %>% data.frame() %>% set_colnames(c('Year', seq(1,12))) 

# Summary of results
y_ind <- h1_mod[[1]]$output[[1]]$SSB[,1] %in% h1_mod[[1]]$output[[1]]$msy_mt[,1]
Table_34 <- data.frame(Year  = h1_mod[[1]]$output[[1]]$msy_mt[,1]) %>% 
                  mutate(
                    "Landings ('000 t)"                       = round(as.vector(rowSums(h1_mod[[1]]$data$Fcaton)), 0),
                    "SSB ('000 t)"                            = round(h1_mod[[1]]$output[[1]]$SSB[y_ind,2], 0),
                    "Recruitment (age 1, millions)"           = round(h1_mod[[1]]$output[[1]]$R[,2], 0),
                    "Fishing Mortality (mean over ages 1-12)" = round(h1_mod[[1]]$output[[1]]$msy_mt[,6], 2),
                    "F_MSY"                                   = round(h1_mod[[1]]$output[[1]]$msy_mt[,5], 2),
                    "SSB_MSY ('000 t)"                        = round(h1_mod[[1]]$output[[1]]$msy_mt[,10], 0),
                    )

y_ind <- h2_mod[[1]]$output[[1]]$SSB[,1] %in% h2_mod[[1]]$output[[1]]$msy_mt[,1]
Table_35 <- data.frame(Year  = h1_mod[[1]]$output[[1]]$msy_mt[,1]) %>% 
                  mutate(
                    "Landings ('000 t)"                       = round(as.vector(rowSums(h2_mod[[1]]$data$Fcaton[,-3])), 0),
                    "SSB ('000 t)"                            = round(h2_mod[[1]]$output[[1]]$SSB[y_ind,2], 0),
                    "Recruitment (age 1, millions)"           = round(h2_mod[[1]]$output[[1]]$R[,2], 0),
                    "Fishing Mortality (mean over ages 1-12)" = round(h2_mod[[1]]$output[[1]]$msy_mt[,6], 2),
                    "F_MSY"                                   = round(h2_mod[[1]]$output[[1]]$msy_mt[,5], 2),
                    "SSB_MSY ('000 t)"                        = round(h2_mod[[1]]$output[[1]]$msy_mt[,10], 0),
                    )

y_ind <- h2_mod[[1]]$output[[2]]$SSB[,1] %in% h2_mod[[1]]$output[[2]]$msy_mt[,1]
Table_36 <- data.frame(Year  = h1_mod[[1]]$output[[1]]$msy_mt[,1]) %>% 
                  mutate(
                    "Landings ('000 t)"                       = round(as.vector(h2_mod[[1]]$data$Fcaton[,3]), 0),
                    "SSB ('000 t)"                            = round(h2_mod[[1]]$output[[2]]$SSB[y_ind,2], 0),
                    "Recruitment (age 1, millions)"           = round(h2_mod[[1]]$output[[2]]$R[,2], 0),
                    "Fishing Mortality (mean over ages 1-12)" = round(h2_mod[[1]]$output[[2]]$msy_mt[,6], 2),
                    "F_MSY"                                   = round(h2_mod[[1]]$output[[2]]$msy_mt[,5], 2),
                    "SSB_MSY ('000 t)"                        = round(h2_mod[[1]]$output[[2]]$msy_mt[,10], 0),
                    )

library(rtf)
if(file.exists(docname)) {file.remove(docname)}
rtffile <- RTF(docname)  

#addPageBreak(rtffile)
addParagraph(rtffile, paste0("Table 1: Input catch (tonnes) by fleet (combined) for the stock assessment model. Note that ",path.m$data$years[2]," data are preliminary.\n"))
addTable(rtffile, round(Table_1, 2))

addPageBreak(rtffile)
addParagraph(rtffile, "Table 2: Input catch at age for fleet 1. Units are relative value (they are normalized to sum to 100 for each year in the model).\n")
addTable(rtffile, round(Table_2))

addPageBreak(rtffile) #,width=11,height=8.5,omi=c(0.5,0.5,0.5,0.5))
addParagraph(rtffile, "Table 3: Input catch at age for fleet 2. Units are relative value (they are normalized to sum to 100 for each year in the model).\n")
addTable(rtffile, round(Table_3))

addPageBreak(rtffile)
addParagraph(rtffile, "Table 4: Input catch at age for fleet 4. Units are relative value (they are normalized to sum to 100 for each year in the model).\n")
addTable(rtffile, round(Table_4))

addPageBreak(rtffile)
addParagraph(rtffile, "Table 5: Input catch at length for fleet 3. Units are relative value (they are normalized to sum to 100 for each year in the model).\n")
addTable(rtffile, round(Table_5))

addPageBreak(rtffile)
addParagraph(rtffile, "Table 6: Survey index values used within the assessment model.\n")
addTable(rtffile, round(Table_6, 3))

addPageBreak(rtffile)
addParagraph(rtffile, "Table 7: Input catch at age for acoustic surveys at southern of Chile. Units are relative value (they are normalized to sum to 100 for each year in the model).\n")
addTable(rtffile, round(Table_7))

addPageBreak(rtffile)
addParagraph(rtffile, "Table 8: Input catch at age for acoustic surveys at northern of Chile. Units are relative value (they are normalized to sum to 100 for each year in the model).\n")
addTable(rtffile, round(Table_8))

addPageBreak(rtffile)
addParagraph(rtffile, "Table 9: Input catch at age for DEPM surveys at southern of Chile. Units are relative value (they are normalized to sum to 100 for each year in the model).\n")
addTable(rtffile, round(Table_9))

addPageBreak(rtffile)
addParagraph(rtffile, "Table 10: Input mean body mass (kg) at age over time assumed for fleet 1.\n")
addTable(rtffile, round(Table_10, 3))

addPageBreak(rtffile)
addParagraph(rtffile, "Table 11: Input mean body mass (kg) at age over time assumed for fleet 2.\n")
addTable(rtffile, round(Table_11, 3))

addPageBreak(rtffile)
addParagraph(rtffile, "Table 12: Input mean body mass (kg) at age over time assumed for fleet 3.\n")
addTable(rtffile, round(Table_12, 3))

addPageBreak(rtffile)
addParagraph(rtffile, "Table 13: Input mean body mass (kg) at age over time assumed for fleet 4. Weight-at-age 1970-2013 were assumed to be the same as fleet 2.\n")
addTable(rtffile, round(Table_13, 3))

addPageBreak(rtffile)
addParagraph(rtffile, paste0("Table 27: Spawning biomass of Jack mackerel (base model under the single-stock hypothesis) estimated in previous SPRFMO SC meetings.\n"))
addTable(rtffile, round(Table_27))

addPageBreak(rtffile)
addParagraph(rtffile, paste0("Table 28: Estimated begin-year numbers at age (Model ", h1nm,"; single-stock hypothesis).\n"))
addTable(rtffile, round(Table_28,2))

addPageBreak(rtffile)
addParagraph(rtffile, paste0("Table 29: Estimated begin-year numbers at age (Model ", h2nm,"; two-stock hypothesis; southern stock).\n"))
addTable(rtffile, round(Table_29,2))

addPageBreak(rtffile)
addParagraph(rtffile, paste0("Table 30: Estimated begin-year numbers at age (Model ", h2nm,"; two-stock hypothesis; far north stock)."))
addTable(rtffile, round(Table_30,2))

addPageBreak(rtffile)
addParagraph(rtffile, paste0("Table 31: Estimated total fishing mortality at age (Model ", h1nm,"; single-stock hypothesis).\n"))
addTable(rtffile, round(Table_31,3))

addPageBreak(rtffile)
addParagraph(rtffile, paste0("Table 32: Estimated total fishing mortality at age (Model ", h2nm,"; two-stock hypothesis; southern stock).\n"))
addTable(rtffile, Table_32)

addPageBreak(rtffile)
addParagraph(rtffile, paste0("Table 33: Estimated total fishing mortality at age (Model ", h2nm,"; two-stock hypothesis; far north stock).\n"))
addTable(rtffile, Table_33)

addPageBreak(rtffile)
addParagraph(rtffile, paste0("Table 34: Summary of results for Model ", h1nm," (single-stock hypothesis). Note that MSY values are a function of time-varying selectivity and average weight. \n"))
addTable(rtffile, Table_34)

addPageBreak(rtffile)
addParagraph(rtffile, paste0("Table 35: Summary of results for Model ", h2nm," (two-stock hypothesis; southern stock). Note that MSY values are a function of time-varying selectivity and average weight.\n"))
addTable(rtffile, Table_35)

addPageBreak(rtffile)
addParagraph(rtffile, paste0("Table 36: Summary of results for Model ", h2nm," (two-stock hypothesis; far north stock). Note that MSY values are a function of time-varying selectivity and average weight.\n"))
addTable(rtffile, Table_36)

done(rtffile)

}



