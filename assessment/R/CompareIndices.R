library(readr)
library(ggplot2)
library(tidyr)
library(dplyr)

# Assumes you're in assessment directory
setwd("../data")

datfile <- read_csv("SC06_Indices.csv") %>%
			gather("Key","Value",Old,New) %>%
			group_by(Index)

ggplot(datfile, mapping=aes(
		x=Year,
		y=Value,
		colour=Key)) +
	geom_line() +
	facet_wrap(Index~Key, scales="free_y",ncol=2,labeller = label_wrap_gen(multi_line=FALSE)) +
	theme_minimal() +
	theme(legend.position="none",
			text = element_text(size = 20))


newdat <- read_csv("SC06_Indices.csv") %>%
#			group_by(Index,Year) %>%
			mutate(RelErr=(Old-New)/New)
ggplot(newdat, mapping=aes(
		x=Year,
		y=RelErr)) +
	geom_line() +
	facet_wrap(~Index, scales="free_y")

