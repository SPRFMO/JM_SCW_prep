## head ------------------------------------------------------

library(jjmR)
library(tidyverse)
library(magrittr)
library(reshape2)
library(ggthemes)
library(dplyr)

setwd(getwd())

construct_input_tables<- function(modname, docname="Input_Tables") {

docname <- paste0(docname,".doc")

jjmoutput <- readJJM(modname, path="config", input="input")

path.m = eval(parse(text=paste0("jjmoutput$",names(jjmoutput))))


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

Table_6 <- path.m$output$Stock_1$pobs_ind_1 %>% data.frame() %>% set_colnames(c('Year', seq(1:12))) 
Table_6[,-1] <- Table_6[,-1]*100

Table_7 <- path.m$output$Stock_1$pobs_ind_2 %>% data.frame() %>% set_colnames(c('Year', seq(1:12))) 
Table_7[,-1] <- Table_7[,-1]*100

Table_8 <- path.m$output$Stock_1$pobs_ind_4 %>% data.frame() %>% set_colnames(c('Year', seq(1:12))) 
Table_8[,-1] <- Table_8[,-1]*100


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


Table_9 <- nj %>% data.frame() %>% set_colnames(c('Year','Chile (1)','Chile (2)','Chile (3)','Chile (4)','Peru(2)',
                                                  'Peru(3)','China','Offshore') )

Table_10 <- path.m$output$Stock_1$wt_fsh_1 %>% data.frame() %>% set_colnames(c('Year', seq(1,12))) 

Table_11 <- path.m$output$Stock_1$wt_fsh_2 %>% data.frame() %>% set_colnames(c('Year', seq(1,12))) 

Table_12 <- path.m$output$Stock_1$wt_fsh_3 %>% data.frame() %>% set_colnames(c('Year', seq(1,12))) 

Table_13 <- path.m$output$Stock_1$wt_fsh_4 %>% data.frame() %>% set_colnames(c('Year', seq(1,12))) 


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
addParagraph(rtffile, "Table 6: Input catch at age for acoustic surveys at southern of Chile. Units are relative value (they are normalized to sum to 100 for each year in the model). Green shading reflects relative level.\n")
addTable(rtffile, round(Table_6))

addPageBreak(rtffile)
addParagraph(rtffile, "Table 7: Input catch at age for acoustic surveys at northern of Chile. Units are relative value (they are normalized to sum to 100 for each year in the model). Green shading reflects relative level.\n")
addTable(rtffile, round(Table_7))

addPageBreak(rtffile)
addParagraph(rtffile, "Table 8: Input catch at age for DEPM surveys at southern of Chile. Units are relative value (they are normalized to sum to 100 for each year in the model). Green shading reflects relative level\n")
addTable(rtffile, round(Table_8))

addPageBreak(rtffile)
addParagraph(rtffile, "Table 9: Survey index values used within the assessment model.\n")
addTable(rtffile, round(Table_9, 3))

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

done(rtffile)

}





