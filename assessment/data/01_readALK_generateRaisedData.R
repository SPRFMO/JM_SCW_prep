# ==================================================================
# SPRFMO Catch template reader (SC09 2021)
#
# 11/10/2019: first coding
# 03/10/2020: finalized coding for 2019 and 2020 data
# 04/10/2020: also available for earlier data now
# 11/08/2021: added the 2020 ALK data from Chile (both old and new method)
# ==================================================================
# Reset lists
rm(list=ls())

# general libraries
library(readxl)        # excel reader from hadley; much quicker than the java based excel readers
library(xlsx)          # reading in excel files and tabs 
library(lubridate)     # data and time functions
library(stringr)       # string manipulation
library(pander)        # for tables
library(reshape2)      # cast and melt dataframes
library(scales)        # pretty scales
library(tidyverse)     # data manipulation and piping
library(doBy)          # ordering of data

# source utilities
onedrive_path <- "C:/Users/hintz001/"
sprfmo_path   <- "South Pacific Regional Fisheries Management Organisation/Teamsite - Age Length Key repository/2021 Data submission"

data_path <- file.path(onedrive_path, sprfmo_path)
# Create list of directories
dir.list <- list.dirs(path=data_path, recursive=TRUE, full.names = TRUE)
dir.list <- dir.list[grepl("ALK",toupper(dir.list))]
dir.list <- dir.list[!grepl("ATTIC$",toupper(dir.list))]
# dir.list <- dir.list[!grepl("ALK$",toupper(dir.list))]
# dir.list <- dir.list[!grepl("prel$|old$",tolower(dir.list))]
# Create file list for import
file.list <- as.character(NA)
for (d in dir.list) {
  file.list   <- c(file.list,
                   list.files(path = d,recursive  = FALSE,pattern = "xls",full.names = TRUE,ignore.case= FALSE) )
}
file.list <- file.list[!grepl("//~", file.list)]
file.list <- file.list[!grepl("ANTIGUO", file.list)]
file.list <- file.list[!grepl("2019", file.list)]
file.list <- file.list[!is.na(file.list)]
# read_catch function -------------------------------------------------------------------
# i <- 5
# ws<-"catch data"
read_catch <- function(file, ws, member, species, year) {
  t1 <-
    suppressMessages(read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", range = "B12:M13")) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE) %>%
    setNames(c("quarter", "year")) %>%
    mutate(quarter = as.numeric(stringr::str_match_all(quarter, "[0-9]+")) ) %>%
    zoo::na.locf(.)                       # fill the empty values with previous values
  t <-
    suppressMessages(read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", range = "B15:M20", .name_repair = "minimal")) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE) %>%
    setNames(c("variable","unit","F1","F2","F3","F4")) %>%
    bind_cols(t1) %>%
    gather(key=fleet, value=value, 3:6) %>%
    filter(!is.na(value)) %>%
    mutate(unit = gsub("[[:punct:]]", "", tolower(unit))) %>%
    mutate(variable = tolower(variable)) %>%
    mutate(member=member, species=species, year=as.integer(year), file=basename(file))  %>%
    mutate(value = as.numeric(value))
  # generate dataframe is not all empty
  if (nrow(t) > 0) {
    print(ws)
    return(t)
  }
} # end of function
# i <- 1
# ws = "sampling"
# file = file.list[i]
# read_sampling function -------------------------------------------------------------------
read_sampling <- function(file, ws, member, species, year) {
  t1 <-
    suppressMessages(read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", range = "B8:Q8")) %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE) %>%
    setNames(c("quarter")) %>%
    mutate(quarter = as.numeric(stringr::str_match_all(quarter, "[0-9]+")),
           quarter = ifelse(quarter > 4, NA, quarter),
           year    = year) %>%
    zoo::na.locf(.)                        # fill the empty values with previous values
  t <-
    read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", range = "B9:Q14", .name_repair = "minimal") %>%
    t() %>%
    data.frame(stringsAsFactors = FALSE) %>%
    setNames(c("variable","unit","F1","F2","F3","F4")) %>%
    bind_cols(t1) %>%
    gather(key=fleet, value=value, 3:6) %>%
    mutate(unit = gsub("[[:punct:]]", "", tolower(unit))) %>%
    mutate(member=member, species=species, year=year, file=basename(file))  %>%
    filter(!is.na(value)) %>%
    mutate(value = as.numeric(value))
  # generate dataframe is not all empty
  if (nrow(t) > 0) {
    print(ws)
    return(t)
  }
} # end of function
# read_length function -------------------------------------------------------------------
read_length <- function(file, ws, member, species, year) {
  # fleet      <- gsub("LEN ","", ws)
  fleet      <- stringr::word(gsub("FLEET ", "F", toupper(ws)), 2)
  lengthtype <- as.character(read_excel(file, sheet = ws, col_names = FALSE, col_types = "text", 
                                        range = "A8", .name_repair = "minimal"))
  lengthunit <- as.character(read_excel(file, sheet = ws, col_names = FALSE, col_types = "text", 
                                        range = "A9", .name_repair = "minimal"))
  unit       <- as.character(read_excel(file, sheet = ws, col_names = FALSE, col_types = "text", 
                                        range = "F5", .name_repair = "minimal"))
  t <-
    suppressMessages(read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", 
                                range = "A10:E109", .name_repair = "minimal")) %>%
    setNames(c("length","quarter1","quarter2","quarter3","quarter4")) %>%
    gather(key=quarter, value=value, quarter1:quarter4) %>%
    mutate(member=member, species=species, year=year, file=basename(file), fleet=fleet, unit=unit,
           lengthtype=lengthtype, lengthunit = lengthunit)  %>%
    mutate(quarter = as.numeric(stringr::str_match_all(quarter, "[0-9]+")) ) %>%
    mutate(value   = as.numeric(gsub("//s+", "", value) )) %>%
    mutate(length  = as.numeric(length)) %>%
    mutate(unit = gsub("[[:punct:]]", "", tolower(unit))) %>%
    mutate(lengthunit = gsub("[[:punct:]]", "", tolower(lengthunit))) %>%
    filter(!is.na(value)) %>%
    mutate(value = ifelse(unit == "individuals", value/1000, value),
           unit  = ifelse(unit == "individuals", "thousands", unit)) %>%
    mutate(value = ifelse(unit == "millions", value*1000, value),
           unit  = ifelse(unit == "millions", "thousands", unit)) %>%
    group_by(member, fleet, species, year, quarter, lengthtype) %>%
    arrange(member, fleet, species, year, quarter, lengthtype, length) %>%
    mutate(prop = value / sum(value)) %>%
    mutate(cumvalue = cumsum(value),
           cumprop = cumsum(prop)) %>%
    ungroup()
  # generate dataframe is not all empty
  if (nrow(t) > 0) {
    print(ws)
    return(t)
  }
} # end of function
# read_alk function -------------------------------------------------------------------
# file       <- file.list[1]
# worksheets <- excel_sheets(file)
# ws         <- worksheets[grepl("ALK", worksheets)][1]
# member     <- as.character(read_excel(file, sheet = 1, col_names = FALSE, col_types = "text", range = "F6"))
# species    <- as.character(read_excel(file, sheet = 1, col_names = FALSE, col_types = "text", range = "F5"))
# year       <- as.integer  (read_excel(file, sheet = 1, col_names = FALSE, col_types = "numeric", range = "K3"))
# i <- 3
# ws = "ALK F2"
# file = file.list[i]
read_alk <- function(file, ws, member, species, year) {
    
  # fleet      <- gsub("ALK ","", ws)
  fleet      <- stringr::word(gsub("FLEET ", "F", toupper(ws)), 2)
  lengthtype <- as.character(read_excel(file, sheet = ws, col_names = FALSE, col_types = "text", 
                                        range = "A10", .name_repair = "minimal") )
  lengthunit <- as.character(read_excel(file, sheet = ws, col_names = FALSE, col_types = "text", 
                                        range = "A11", .name_repair = "minimal") )
  # ages
  a <-
    suppressMessages(read_excel(file, sheet = ws, col_names = FALSE, col_types = "text", range = "B11:N11"))  %>%
    mutate_all( ~ paste0("age",.)) %>%
    as.character()
  t1 <-
    suppressMessages(read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", 
                                range = "A12:N111", .name_repair = "minimal")) %>%
    setNames(c("length",all_of(a))) %>%
    mutate(quarter=1) %>%
    gather(key=age, value=value, all_of(a)) %>%
    filter(!is.na(value)) %>%
    mutate(value   = as.numeric(gsub("//s+", "", value) ))
  t2 <-
    suppressMessages(read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", 
                                range = "P12:AC111", .name_repair = "minimal")) %>%
    setNames(c("length",all_of(a))) %>%
    mutate(quarter=2) %>%
    gather(key=age, value=value, all_of(a)) %>%
    filter(!is.na(value)) %>%
    mutate(value   = as.numeric(gsub("//s+", "", value) ))
  t3 <-
    suppressMessages(read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", 
                                range = "AE12:AR111", .name_repair = "minimal")) %>%
    setNames(c("length",all_of(a))) %>%
    mutate(quarter=3) %>%
    gather(key=age, value=value, all_of(a)) %>%
    filter(!is.na(value)) %>%
    mutate(value   = as.numeric(gsub("//s+", "", value) ))
  t4 <-
    suppressMessages(read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", 
                                range = "AT12:BG111", .name_repair = "minimal")) %>%
    setNames(c("length",all_of(a))) %>%
    mutate(quarter=4) %>%
    gather(key=age, value=value, all_of(a)) %>%
    filter(!is.na(value)) %>%
    mutate(value   = as.numeric(gsub("//s+", "", value) ))
  t <-
    bind_rows(t1, t2, t3, t4) %>%
    mutate(member=member, species=species, year=year, file=basename(file), fleet=fleet,
           lengthtype=lengthtype, lengthunit = lengthunit)  %>%
    mutate(value   = as.numeric(gsub("//s+", "", value) )) %>%
    mutate(length  = as.numeric(length)) %>%
    mutate(age     = as.integer(gsub("age","",age))) %>%
    mutate(lengthunit = gsub("[[:punct:]]", "", tolower(lengthunit))) %>%
    group_by(member, fleet, species, year, quarter, length) %>%
    mutate(prop = value / sum(value)) %>%
    ungroup()
  # generate dataframe is not all empty
  if (nrow(t) > 0) {
    print(ws)
    return(t)
  }
} # end of function
# read_canum function -------------------------------------------------------------------
# ws <- "CN F1"
read_canum <- function(file, ws, member, species, year) {
  # fleet      <- gsub("CN ","", ws)
  fleet      <- stringr::word(gsub("FLEET ", "F", toupper(ws)), 2)
  unit       <- as.character(read_excel(file, sheet = ws, col_names = FALSE, col_types = "text", range = "C11:N11", .name_repair = "minimal") )
  t <-
    suppressMessages(read_excel(file, sheet = ws, col_names = FALSE, na="NA", col_types = "text", range = "B12:N24", .name_repair = "minimal")) %>%
    setNames(c("age","canum_1","meanlength_1","meanweight_1","canum_2","meanlength_2","meanweight_2",
                     "canum_3","meanlength_3","meanweight_3","canum_4","meanlength_4","meanweight_4")) %>%
    mutate(age = as.integer(gsub("//+","",age))) %>%
    pivot_longer(names_to = "tosplit", values_to="value", canum_1:meanweight_4) %>%
    mutate(unit=rep(unit,13)) %>%
    separate(tosplit, into=c("variable","quarter"), sep="_") %>%
    filter(!is.na(value)) %>%
    mutate(unit = gsub("[[:punct:]]", "", tolower(unit))) %>%
    mutate(member=member, species=species, year=year, file=basename(file), fleet=fleet)  %>%
    mutate(value   = as.numeric(gsub("//s+", "", value) )) %>%
    mutate(value = ifelse(unit == "individuals", value/1000, value),
           unit  = ifelse(unit == "individuals", "thousands", unit)) %>%
    mutate(value = ifelse(unit == "millions", value*1000, value),
           unit  = ifelse(unit == "millions", "thousands", unit)) %>%
    arrange(variable, quarter, age) %>%
    ungroup()
  # generate dataframe is not all empty
  if (nrow(t) > 0) {
    print(ws)
    return(t)
  }
} # end of function
# i    <- 1
# file <- file.list[i]
# ws   <- "catch data"
# create empty dataframes
catch <- sampling <- length <- alk <- alk2 <- canum <- data.frame(stringsAsFactors = FALSE)
# i <- 8
# start file reading loop
for (i in 1:length(file.list)){
  # get name and number of worksheets in the file
  file        <- file.list[i]
  worksheets  <- readxl::excel_sheets(file)
  nworksheets <- length(worksheets)
  print(paste(i, basename(file), sep=" "))
  # generic information
  member <- as.character(read_excel(file, sheet = "START HERE", col_names = FALSE, col_types = "text", 
                                    range = "F6", .name_repair = "minimal"))
  member <- toupper(member)
  species<- as.character(read_excel(file, sheet = "START HERE", col_names = FALSE, col_types = "text", 
                                    range = "F5", .name_repair = "minimal"))
  year   <- as.integer  (read_excel(file, sheet = "START HERE", col_names = FALSE, col_types = "numeric", 
                                    range = "K3", .name_repair = "minimal"))
  # catch
  for (ws in worksheets[grepl("catch", worksheets)]) {
    catch <- bind_rows(catch, read_catch(file=file, ws=ws, member=member, species=species, year=year)) }
  # sampling
  for (ws in worksheets[grepl("sampling", worksheets)]) {
    sampling <- bind_rows(sampling, read_sampling(file=file, ws=ws, member=member, species=species, year=year)) }
  # length
  for (ws in worksheets[grepl("LEN", worksheets)]) {
    if(!grepl("TL", toupper(ws))) {
      length <- bind_rows(length, read_length(file=file, ws=ws, member=member, species=species, year=year)) }
    }
  
  # alk
  for (ws in worksheets[grepl("ALK", worksheets)]) {
    if(!grepl("TL", toupper(ws))) {
      alk <- bind_rows(alk, read_alk(file=file, ws=ws, member=member, species=species, year=year)) }
  }
  
  # canum
  for (ws in worksheets[grepl("CN|CANUM", worksheets)]) {
    canum <- bind_rows(canum, read_canum(file=file, ws=ws, member=member, species=species, year=year)) }
} # end of i loop
# summarise to avoid any splits in files (e.g. Russia 2019)
catch    <- catch %>% group_by(variable, unit, quarter, year, fleet, member, species) %>% summarise(value = sum(value, na.rm=TRUE)) %>% ungroup()
sampling <- sampling %>% group_by(variable, unit, quarter, year, fleet, member, species) %>% summarise(value = sum(value, na.rm=TRUE)) %>% ungroup()
length   <- 
  length %>% 
  mutate(lengthtype = gsub(" ","", tolower(lengthtype))) %>% 
  group_by(unit, quarter, year, fleet, member, species, lengthtype, lengthunit, length) %>% 
  summarise(value = sum(value, na.rm=TRUE)) %>% 
  group_by(member, fleet, species, year, quarter, lengthtype) %>%
  arrange(member, fleet, species, year, quarter, lengthtype, length) %>%
  mutate(prop = value / sum(value)) %>%
  mutate(cumvalue = cumsum(value),
         cumprop = cumsum(prop)) %>%
  ungroup()
# canum
t1 <- canum %>% filter(variable %in% c("canum")) %>% 
  group_by(variable, unit, quarter, year, fleet, member, species, age) %>% 
  summarise(value = mean(value, na.rm=TRUE))
t2 <- canum %>% filter(variable %in% c("meanlength","meanweight")) %>% 
  group_by(variable, unit, quarter, year, fleet, member, species, age) %>% 
  summarise(value = mean(value, na.rm=TRUE))
canum <- bind_rows(t1,t2)
alk <- 
  bind_rows(alk, alk2) %>%
  mutate(lengthtype = gsub(" ","", tolower(lengthtype))) %>% 
  mutate(member = gsub("CHILE","Chile", member)) %>% 
  group_by(lengthtype, lengthunit, quarter, year, fleet, member, species, age, length) %>% 
  summarise(value = sum(value, na.rm=TRUE)) %>% 
  group_by(lengthtype, lengthunit, member, fleet, species, year, quarter, length) %>%
  mutate(prop = value / sum(value)) %>%
  ungroup()
  

#- Fix plusgroup age to numeric value of 12
canum$age[which(is.na(canum$age))] <- 12

# save(catch, sampling, length, alk, canum, file=file.path("D:/TEMP",fn))
save(catch, sampling, length, alk, canum, file=file.path(data_path,paste0("SC09_data_",format(Sys.time(), '%Y%m%d'), ".RData")))

#SC09 Jack Mackerel Working Group data inputs

outPath <- "South Pacific Regional Fisheries Management Organisation/SPRFMO SC9 - Jack Mackerel/04 Data/"

#- Total catch by fleet
totalCatch <- read.xlsx(file.path(onedrive_path,"South Pacific Regional Fisheries Management Organisation/SPRFMO SC9 - Jack Mackerel/01 Meeting documents/SC9-JM01 Annex 1 CJM catch history data.xlsx"),sheetName="CJM Stock assess input",rowIndex=c(1,37:88))
colnames(totalCatch)[c(2,3,9,21,22)] <- c("Fleet_1","Fleet_2","Fleet_3","Fleet_4","Fleet_total")
cols2keep  <- c("Participant",colnames(totalCatch)[grep("Fleet_",colnames(totalCatch))])
totalCatch  <- totalCatch[,cols2keep]
write.csv(totalCatch,file=file.path(onedrive_path,outPath,"SC09_catchByFleet.csv"))

#- Catch numbers at age by fleet
canumN    <- subset(canum,unit=="thousands")
canumN    <- aggregate(canumN$value,by=as.list(canumN[,c("year","fleet","age")]),FUN=sum,na.rm=T)

canumNmat <- pivot_wider(canumN,names_from=age,values_from=x)
canumNmat <- orderBy(~fleet+year,data=canumNmat)
write.csv(canumNmat,file=file.path(onedrive_path,outPath,"SC09_catchNumByFleet_newAgeMet.csv"))

#- Catch weight at age by fleet
canumWt   <- subset(canum,unit=="kg")
canumN    <- subset(canum,unit=="thousands")
colnames(canumWt)[grep("value",colnames(canumWt))] <- "value_weight"
canumWt   <- merge(canumWt,canumN,by=c("year","quarter","fleet","member","age"))
canumWt   <- aggregate(canumWt$value * canumWt$value_weight,by=as.list(canumWt[,c("year","fleet","age")]),FUN=sum,na.rm=T)
canumWtmat <- pivot_wider(canumWt,names_from=age,values_from=x)
canumWtmat <- orderBy(~fleet+year,data=canumWtmat)
canumWtmat[,-c(1,2)] <- canumWtmat[,-c(1,2)]/canumNmat[,-c(1,2)]
write.csv(canumWtmat,file=file.path(onedrive_path,outPath,"SC09_catchWtByFleet_newAgeMet.csv"))

#- Catch length by fleet
lengthN   <- aggregate(length$value,by=as.list(length[,c("year","fleet","length")]),FUN=sum,na.rm=T)
lengthNmat <- pivot_wider(lengthN,names_from=length,values_from=x)
lengthNmat <- orderBy(~fleet+year,data=lengthNmat)
write.csv(lengthNmat,file=file.path(onedrive_path,outPath,"SC09_catchLengthByFleet.csv"))

