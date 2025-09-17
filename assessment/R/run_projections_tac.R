# Need to be in separate directory where runs will be conducted
# h1 and h2 should be in different directories as results files will be over-written

tac <- 1428
tac15 <- tac * 1.15
tac25 <- 1785.375
tac100 <- tac * 2

h <- "h2"
finmodnm <- "1.14"

# To get .bar file
system(paste0("./jjm -ind ", h, "_", finmodnm, ".ls.ctl"))
system(paste0("./jjm -ind ", h, "_", finmodnm, ".ls.ctl -binp jjm.b02"))

system(paste0("./jjm -ind ", h, "_", finmodnm, ".ls.ctl -binp jjm.bar -phase 22 -fut_sel 3 -tac ", tac, " -sdonly"))
file.rename(from="For_R_1.rep", to=paste0(h,"_For_R_1_tac.rep"))
if(h=="h2") file.rename(from="For_R_2.rep", to=paste0(h,"_For_R_2_tac.rep"))

system(paste0("./jjm -ind ", h, "_", finmodnm, ".ls.ctl -binp jjm.bar -phase 22 -fut_sel 3 -tac ", tac15, " -sdonly"))
file.rename(from="For_R_1.rep", to=paste0(h,"_For_R_1_tac15.rep"))
if(h=="h2") file.rename(from="For_R_2.rep", to=paste0(h,"_For_R_2_tac15.rep"))

system(paste0("./jjm -ind ", h, "_", finmodnm, ".ls.ctl -binp jjm.bar -phase 22 -fut_sel 3 -tac ", tac20, " -sdonly"))
file.rename(from="For_R_1.rep", to=paste0(h,"_For_R_1_tac20.rep"))
if(h=="h2") file.rename(from="For_R_2.rep", to=paste0(h,"_For_R_2_tac20.rep"))

system(paste0("./jjm -ind ", h, "_", finmodnm, ".ls.ctl -binp jjm.bar -phase 22 -fut_sel 3 -tac ", tac25, " -sdonly"))
file.rename(from="For_R_1.rep", to=paste0(h,"_For_R_1_tac25.rep"))
if(h=="h2") file.rename(from="For_R_2.rep", to=paste0(h,"_For_R_2_tac25.rep"))

system(paste0("./jjm -ind ", h, "_", finmodnm, ".ls.ctl -binp jjm.bar -phase 22 -fut_sel 3 -tac ", tac100, " -sdonly"))
file.rename(from="For_R_1.rep", to=paste0(h,"_For_R_1_tac100.rep"))
if(h=="h2") file.rename(from="For_R_2.rep", to=paste0(h,"_For_R_2_tac100.rep"))


# Copypasta tac.rep files to jjm/assessment/risk_tables
#---- Reading in results

library(here)
library(tidyverse)
devtools::load_all("../../jjmR")
source("R/read-admb.R")

finmodnm <- "1.14"
yr_curr <- as.numeric(format(Sys.time(), "%Y"))
h1_modls <- readJJM(paste0("h1_",finmodnm,".ls"), path = "config", input = "input") %>% fixed_bmsy()
h2_modls <- readJJM(paste0("h2_",finmodnm,".ls"), path = "config", input = "input") %>% fixed_bmsy()

h1_mod <- readJJM(paste0("h1_",finmodnm), path = "config", input = "input") %>% fixed_bmsy()
h2_mod <- readJJM(paste0("h2_",finmodnm), path = "config", input = "input") %>% fixed_bmsy()

mods_com_ls <- combineModels(h1_modls,h2_modls)
mods_com <- combineModels(h1_mod,h2_mod)

quants_hist <- mods_com_ls %>%
  .reshapeJJM2(what="catchProj") %>%
  rename(catch=data) %>%
  left_join(
    mods_com_ls %>%
    .reshapeJJM2(what="ssbProj") %>%
    rename(ssb=data)
  ) %>%
  mutate(stocks=str_remove(stocks,"Stock_"),
    model=str_remove(model,paste0("_",finmodnm,".ls"))) %>%
  rename(stock=stocks, hyp=model)

quants_msy <- mods_com %>%
  get_msy_mt() %>%
  group_by(model, stock) %>%
  summarise(bmsy=unique(bmsy)) %>%
  ungroup() %>%
  mutate(hyp=str_remove(model,paste0("_",finmodnm)),
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

fn_pullfutF <- function(nm, scen) {
  tmp <- read_rep(here("risk_tables",nm))

  ind_cat <- grep(paste0("Catch_fut_", scen), names(tmp))
  ind_ssb <- grep(paste0("SSB_fut_", scen), names(tmp))

  cat_fut <- tmp[[ind_cat]] %>%
    as_tibble() %>%
    rename(year=V1,catch=V2)

  ssb_fut <- tmp[[ind_ssb]] %>%
    as_tibble() %>%
    rename(year=V1, ssb=V2, sd=V3, lb=V4, ub=V5)

  if(scen == 2) scn <- 1.5
  if(scen == 3) scn <- 2

  quant_fut <- full_join(cat_fut,ssb_fut) %>%
    mutate(file=nm, scenario = paste0("F",yr_curr," x",scn)) %>%
     separate(file, into=c("hyp","file"), sep="_For_R_") %>%
     mutate(stock = str_remove(file, "_tac.rep")) %>%
     select(-file)

  return(quant_fut)
}

filenamesF <- repfilenames[grep("_tac.rep", repfilenames)]

quants_fut <- map_dfr(repfilenames, fn_pullfuts) %>%
  separate(file, into=c("hyp","file"), sep="_For_R_") %>%
  separate(file, into=c("stock","scenario"), sep="_") %>%
  mutate(
    scenario = str_remove(scenario,".rep"),
    scenario = str_replace(scenario, "tac", paste0("TAC",yr_curr, " x1."))
)

  # %>%
  # bind_rows(map_dfr(filenamesF, fn_pullfutF, scen = 2)) %>%
  # bind_rows(map_dfr(filenamesF, fn_pullfutF, scen = 3))

quants_proj_all <- quants_fut %>%
  select(year, catch, ssb, sd, hyp, stock, scenario) %>%
  bind_rows(quants_hist)

quants_rm <- quants_proj_all %>%
  filter((year <= yr_curr & scenario != paste0("F", yr_curr, " SQ")) |
  		str_detect(scenario, "FTAC"))

quants_curr <- quants_proj_all %>%
  filter(year==yr_curr | year==yr_curr+1) %>%
  complete(year,hyp,stock,scenario) %>%
  filter(year==yr_curr) %>%
  arrange(year,hyp,stock) %>%
  group_by(year,hyp,stock) %>%
  fill(everything(),.direction="updown") %>%
  drop_na(catch, ssb) %>%
  ungroup()

quants_proj <- quants_proj_all %>%
  anti_join(quants_rm) %>%
  bind_rows(quants_curr) %>%
  distinct()    %>%
  mutate(scenario = case_when(
    scenario == "TAC2025 x1." ~ "ADV2025",
    scenario == "TAC2025 x1.15" ~ "ADV2025 x15",
    scenario == "TAC2025 x1.25" ~ "TAC2025 x1.15",
    .default = scenario
  )
)


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
  mutate(scenario = case_when(
    scenario == "TAC2025 x1." ~ "ADV2025",
    scenario == "TAC2025 x1.15" ~ "ADV2025 x1.15",
    scenario == "TAC2025 x1.25" ~ "TAC2025 x1.15",
    .default = scenario
    )
  ) %>%
  arrange(hyp, stock, scenario) %>%
  write_csv("risk_tables/tac.csv")

ggplot(quants_proj %>% filter(year>=2000)) +
	geom_line(aes(x=year,y=catch,colour=scenario), alpha = 0.8) +
	facet_grid(stock~hyp, scales="free_y") +
  ylab("Catch ('000 t)") +
  xlab("Year") +
	theme_jjm() +
  scale_color_viridis_d(option="turbo")

ggplot(quants_proj %>% filter(year>=2000)) +
	geom_line(aes(x=year,y=ssb,colour=scenario)) +
	facet_grid(stock~hyp, scales="free_y") +
	ylab("SSB ('000 t)") +
  xlab("Year") +
  theme_jjm() +
  scale_color_viridis_d(option="turbo")
