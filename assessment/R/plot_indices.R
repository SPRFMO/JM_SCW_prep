# Index Workup
# Assumes you're in jjm/assessment
library(tidyverse)
library(readr)
rawdat <- read_csv("../data/SC07_Indices.csv")


rawdat %>%
	select(starts_with("Index")) %>%
	names() %>%
	str_remove(.,"Index_") %>%
	lapply(function(x){unite(rawdat, !!x, grep(x, names(rawdat), value = TRUE), 
                            sep = '_', remove = TRUE) %>% select(x)}) %>%
  	bind_cols() %>%
  	pivot_longer(everything(),
  					names_to="Source") %>%
  	separate(value,into=c("Year","Value"),sep="_") %>%
  	mutate(Year=as.numeric(Year),Value=as.numeric(Value)) %>%
  	drop_na() %>%
  	group_by(Source) %>%
  	mutate(Normalised_Value=(Value-mean(Value,na.rm=T))/mean(Value,na.rm=T)) %>%
  	filter(Year>2000) %>%
  	ggplot(mapping=aes(x=Year,y=Normalised_Value,colour=Source)) +
  		geom_line()