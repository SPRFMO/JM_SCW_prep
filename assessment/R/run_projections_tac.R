# Need to be in separate directory where runs will be conducted

tac <- 1242
tac15 <- tac*1.15
tac20 <- tac*1.2

h <- "h1"
finmodnm <- "1.07"

# To get .bar file
system(paste0("./jjm -ind ", h, "_", finmodnm, ".ls.ctl"))

system(paste0("./jjm -ind ", h, "_", finmodnm, ".ls.ctl -binp jjm.bar -phase 22 -tac ", tac, " -sdonly"))
file.rename(from="For_R_1.rep", to=paste0(h,"_For_R_1_tac.rep"))
if(h=="h2") file.rename(from="For_R_2.rep", to=paste0(h,"_For_R_2_tac.rep"))

system(paste0("./jjm -ind ", h, "_", finmodnm, ".ls.ctl -binp jjm.bar -phase 22 -tac ", tac15, " -sdonly"))
file.rename(from="For_R_1.rep", to=paste0(h,"_For_R_1_tac15.rep"))
if(h=="h2") file.rename(from="For_R_2.rep", to=paste0(h,"_For_R_2_tac15.rep"))

system(paste0("./jjm -ind ", h, "_", finmodnm, ".ls.ctl -binp jjm.bar -phase 22 -tac ", tac20, " -sdonly"))
file.rename(from="For_R_1.rep", to=paste0(h,"_For_R_1_tac20.rep"))
if(h=="h2") file.rename(from="For_R_2.rep", to=paste0(h,"_For_R_2_tac20.rep"))

#---- Reading in results

library(here)
library(tidyverse)
devtools::load_all("../../jjmR")
source("R/read-admb.R")

finmodnm <- "1.07"
yr_curr <- as.numeric(format(Sys.time(), "%Y"))
h1_modls <- readJJM(paste0("h1_",finmodnm,".ls"), path = "config", input = "input")
h2_modls <- readJJM(paste0("h2_",finmodnm,".ls"), path = "config", input = "input")

mods_com <- combineModels(h1_modls,h2_modls)
quants_hist <- mods_com %>% .reshapeJJM2(what="catchProj") %>%
				rename(catch=data) %>%
				left_join(mods_com %>% .reshapeJJM2(what="ssbProj") %>% rename(ssb=data)) %>%
				mutate(stocks=str_remove(stocks,"Stock_"),
						model=str_remove(model,paste0("_",finmodnm,".ls"))) %>%
				rename(stock=stocks, hyp=model)

quants_msy <-  mods_com %>%
				get_msy_mt() %>%
				filter(year>=(max(year)-9)) %>%
				group_by(model, stock) %>%
				summarise(bmsy=mean(bmsy)) %>%
				ungroup() %>%
				mutate(hyp=str_remove(model,paste0("_",finmodnm,".ls")),
						stock=str_remove(stock,"Stock_")) %>%
				select(-model) %>%
				as_tibble()

repfilenames <- list.files(here("risk_tables"))[grep(".rep",list.files(here("risk_tables")))]

fn_pullfuts <- function(nm) {
	tmp <- read_rep(here("risk_tables",nm))

	ind_cat <- grep("Catch_fut_5",names(tmp))
	ind_ssb <- grep("SSB_fut_5",names(tmp))

	cat_fut <- tmp[[ind_cat]] %>%
				as_tibble() %>%
				rename(year=V1,catch=V2)

	ssb_fut <- tmp[[ind_ssb]] %>%
				as_tibble() %>%
				rename(year=V1, ssb=V2, sd=V3, lb=V4, ub=V5)

	quant_fut <- full_join(cat_fut,ssb_fut) %>%
					mutate(file=nm)

	return(quant_fut)
}

quants_fut <- map_dfr(repfilenames, fn_pullfuts) %>%
			separate(file, into=c("hyp","file"), sep="_For_R_") %>%
			separate(file, into=c("stock","scenario"), sep="_") %>%
			mutate(scenario=str_remove(scenario,".rep")) %>%
			mutate(scenario=case_when(scenario=="tac15" ~ paste0("TAC",yr_curr, " 1.15x"),
										scenario=="tac20" ~ paste0("TAC",yr_curr, " 1.2x"),
										scenario=="tac" ~ paste0("TAC",yr_curr)))

quants_proj <- quants_fut %>%
				select(year, catch, ssb, sd, hyp, stock, scenario) %>%
				bind_rows(quants_hist)

quants_rm <- quants_proj %>%
				filter(year <= yr_curr, scenario!=paste0("F",yr_curr, " SQ"))

quants_curr <- quants_proj %>%
				filter(year==yr_curr | year==yr_curr+1) %>%
				complete(year,hyp,stock,scenario) %>%
				filter(year==yr_curr) %>%
				arrange(year,hyp,stock) %>%
				group_by(year,hyp,stock) %>%
				fill(everything(),.direction="updown") %>%
				drop_na() %>%
				ungroup()

quants_proj <- quants_proj %>%
				anti_join(quants_rm) %>%
				bind_rows(quants_curr) %>%
				distinct()

yrs2use <- c(yr_curr+2, yr_curr+6, yr_curr+10)

risk_table_cat <- quants_fut %>%
					select(year, catch, hyp, stock, scenario) %>%
					filter(year==yr_curr+1 | year==yr_curr+2) %>%
					mutate(catch=round(catch)) %>%
					pivot_wider(names_from=year,values_from=catch,names_prefix="catch_")


risk_table <- quants_fut %>%
				select(year, ssb, sd, hyp, stock, scenario) %>%
				filter(year %in% yrs2use) %>%
				left_join(quants_msy) %>%
				mutate(prob=round((1 - pnorm(bmsy, ssb, sd))*100),
						ssb=round(ssb)) %>%
				select(-bmsy, -sd) %>%
				pivot_wider(names_from=year,values_from=c(ssb,prob), names_sep="_",names_vary="slowest") %>%
				left_join(risk_table_cat) %>%
				write_csv("risk_tables/tac.csv")

ggplot(quants_proj %>% filter(year>=2000)) +
	geom_line(aes(x=year,y=catch,colour=scenario)) +
	facet_grid(stock~hyp, scales="free_y") +
	theme_jjm()

ggplot(quants_proj %>% filter(year>=2000)) +
	geom_line(aes(x=year,y=ssb,colour=scenario)) +
	facet_grid(stock~hyp, scales="free_y") +
	theme_jjm()
