library(jjmR)
library(PBSadmb)
library(tidyverse)
library(ggthemes)


#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000
# summarize H1 and H2
#0000000000000000000000000000000000000000000000000000000000000000000000000000000000000

finmodname_h1 <- geth("1.14", h="h1")
finmodname_h2 <- geth("1.14", h="h2")

finmodnm <- "1.14"

# Was using mod.ls for previous SCs, changed in SC12
mod_h1 <- readJJM(finmodname_h1,path="config",input="input") %>% fixed_bmsy()
mod_h2 <- readJJM(finmodname_h2,path="config",input="input") %>% fixed_bmsy()

res <- combineModels(mod_h1, mod_h2)
res_tidy <- tidy_JJM(res)

mt <- res_tidy$msy_mt %>%
  select(model, stock, year, f) %>%
  rename(value = f) %>%
  mutate(metric = "f")

msy <- res_tidy$msy_mt %>%
  select(model, stock, year, fmsy, bmsy) %>%
  pivot_longer(fmsy:bmsy, names_to = "metric", values_to = "msy") %>%
  mutate(metric = str_remove(metric, "msy"))

rec <- res_tidy$totals %>%
  filter(metric %in% c("R", "SSB")) %>%
  mutate(metric = case_when(
    metric == "R" ~ "rec",
    metric == "SSB" ~ "b"
    )
  ) %>%
  select(model, stock, year, metric, value, lowerbound, upperbound)

tb <- mt %>%
  bind_rows(rec) %>%
  left_join(msy) %>%
  filter(year >= 1970) %>%
  mutate(
    stock = ifelse(
      str_detect(model, "h1"),
      "comb",
      ifelse(
        str_detect(stock, "1"),
        "south",
        "north"
      )
    )
  ) %>%
  mutate(
    model = ifelse(
      str_detect(model, "h1"),
      "h1 (one stock)",
      "h2 (two stock)"
    )
  ) %>%
  mutate(
    refpoint = case_when(
      stock == "comb" & metric == "f" ~ "Fmsy",
      stock == "comb" & metric == "b" ~ "Bmsy",

      stock == "north" & metric == "f" ~ "Fmsy north",
      stock == "north" & metric == "b" ~ "Bmsy north",

      stock == "south" & metric == "f" ~ "Fmsy south",
      stock == "south" & metric == "b" ~ "Bmsy south",
    )
  ) %>%
  mutate(
    metric = case_when(
      metric == "rec" ~ "Recruitment",
      metric == "f" ~ "Fishing Mortality",
      metric == "b" ~ "SSB"
    )
  ) %>%
  as_tibble()

tb_max <- tb %>%
  filter(year >= 2000) %>%
  group_by(model, stock, metric) %>%
  filter(value == max(value))


tb %>%
  filter(year >= 1970) %>%
  ggplot() +
    geom_ribbon(aes(x = year, ymin = lowerbound, ymax = upperbound, fill = stock), alpha = 0.4) +
    geom_line(aes(x = year, y = value, colour = stock)) +
    geom_line(aes(x = year, y = msy, colour = stock), linetype = 2) +
    facet_grid(factor(metric, levels = c("SSB", "Recruitment", "Fishing Mortality"))~model, scales = "free_y") +
    ggrepel::geom_text_repel(data = tb %>% filter(year == 1980), aes(x = year, y = value, label = refpoint, colour = stock)) +
    ggrepel::geom_text_repel(data = tb_max, aes(x = year, y = value, label = stock, colour = stock)) +
    ggthemes::scale_color_colorblind() +
    ggthemes::scale_fill_colorblind() +
    theme_jjm() +
    theme(legend.position = "none")
