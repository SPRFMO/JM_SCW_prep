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
mod_h1 <- readJJM(finmodname_h1,path="config",input="input")
mod_h2 <- readJJM(finmodname_h2,path="config",input="input")

res <- compareModels(geth(finmodnm, c("h1","h2")))
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
  )

rp <-
            data.frame(Hyp="H1 (one stock)", Model="LS, SS", Stock="comb" ,Var="Fishing mortality", Refpoint="Fmsy",
                       Year=mod_h1[[1]]$output$Stock_1$msy_mt[,1],
                       Value=mod_h1[[1]]$output$Stock_1$msy_mt[,5]) %>%
  bind_rows(data.frame(Hyp="H2 (two stock)", Model="LS, SS", Stock="south",Var="Fishing mortality", Refpoint="Fmsy south",
                       Year =mod_h2[[1]]$output$Stock_1$msy_mt[,1],
                       Value=mod_h2[[1]]$output$Stock_1$msy_mt[,5])) %>%
  bind_rows(data.frame(Hyp="H2 (two stock)", Model="LS, SS", Stock="north",Var="Fishing mortality", Refpoint="Fmsy north",
                       Year =mod_h2[[1]]$output$Stock_2$msy_mt[,1],
                       Value=mod_h2[[1]]$output$Stock_2$msy_mt[,5])) %>%

  bind_rows(data.frame(Hyp="H1 (one stock)", Model="LS, SS", Stock="comb" ,Var="SSB", Refpoint="Bmsy",
                       Year=mod_h1[[1]]$output$Stock_1$msy_mt[,1],
                       Value=fixed_bmsy(mod_h1)[[1]]$output$Stock_1$msy_mt[,10])) %>%
  bind_rows(data.frame(Hyp="H2 (two stock)", Model="LS, SS", Stock="south",Var="SSB", Refpoint="Bmsy south",
                       Year =mod_h2[[1]]$output$Stock_1$msy_mt[,1],
                       Value=fixed_bmsy(mod_h2)[[1]]$output$Stock_1$msy_mt[,10])) %>%
  bind_rows(data.frame(Hyp="H2 (two stock)", Model="LS, SS", Stock="north",Var="SSB", Refpoint="Bmsy north",
                       Year =mod_h2[[1]]$output$Stock_2$msy_mt[,1],
                       Value=fixed_bmsy(mod_h2)[[1]]$output$Stock_2$msy_mt[,10]))


# assessment
mdf <-
            data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="SSB",
                       mod_h1[[1]]$output[[1]]$SSB) %>%
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south",Var="SSB",
                       mod_h2[[1]]$output[[1]]$SSB)) %>%
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north",Var="SSB",
                       mod_h2[[1]]$output[[2]]$SSB)) %>%

  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Recruitment",
                       mod_h1[[1]]$output[[1]]$R)) %>%
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south",Var="Recruitment",
                       mod_h2[[1]]$output[[1]]$R)) %>%
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north",Var="Recruitment",
                       mod_h2[[1]]$output[[2]]$R)) %>%

  bind_rows(
      bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Fishing mortality",
                         mod_h1[[1]]$output[[1]]$msy_mt[,c(1,6)])) %>%
      bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south",Var="Fishing mortality",
                           mod_h2[[1]]$output[[1]]$msy_mt[,c(1,6)])) %>%
      bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north",Var="Fishing mortality",
                           mod_h2[[1]]$output[[2]]$msy_mt[,c(1,6)])) %>%
      setNames(gsub("year","X1",names(.))) %>%
      setNames(gsub("f","X2",names(.)))
  ) %>%

  setNames( c("Hyp", "Scenario","Stock","Var", "Year","Value","SD","lb","ub")) %>%
  mutate(Var = factor(Var, levels=c("Catch", "SSB (kt)","Fishing mortality","Recruitment (numbers)"))) %>%
  mutate(Stock = tolower(Stock)) %>%
  mutate(Hyp = ifelse(Hyp=="H1", "H1 (one stock)", "H2 (two stock)"))
# %>%
#   mutate(Stock = ifelse(Stock=="comb","", Stock))

# data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Fishing mortality",
#                      mod_h1[[1]]$output[[1]]$msy_mt[,c(1,6)])


# forecast (NOT USED)
fc <-
            data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="SSB", Fmult="1.0",
                       mod_h1[[1]]$output[[1]]$SSB_fut_1)%>%
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="SSB", Fmult="0.75",
                       mod_h1[[1]]$output[[1]]$SSB_fut_2))%>%
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="SSB", Fmult="1.25",
                       mod_h1[[1]]$output[[1]]$SSB_fut_3))%>%
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="SSB", Fmult="FMSY",
                       mod_h1[[1]]$output[[1]]$SSB_fut_4))%>%
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="SSB", Fmult="0.0",
                       mod_h1[[1]]$output[[1]]$SSB_fut_5))%>%

  # Need to put the Fs into a forecast string with year and value
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Fishing mortality", Fmult="1.0",
                       X2 =1.00*tail(mod_h1[[1]]$output[[1]]$msy_mt[,c(6)],1) ))%>%
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Fishing mortality", Fmult="0.75",
                       X2=0.75*tail(mod_h1[[1]]$output[[1]]$msy_mt[,c(6)],1) ))%>%
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Fishing mortality", Fmult="1.25",
                       X2=1.25*tail(mod_h1[[1]]$output[[1]]$msy_mt[,c(6)],1) ))%>%
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Fishing mortality", Fmult="FMSY",
                       X2=NA))%>%
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Fishing mortality", Fmult="0.0",
                       X2=0.00 ))%>%

  setNames( c("Hyp", "Scenario","Stock","Var", "Fmult", "Year","Value","SD","lb","ub")) %>%
  mutate(Var = factor(Var, levels=c("Catch", "SSB (kt)","Fishing Mortality","Recruitment (numbers)"))) %>%
  mutate(Hyp = ifelse(Hyp=="H1", "H1 (one stock)", "H2 (two stock)"))  %>%
  mutate(Stock = ifelse(Stock=="comb","", Stock))

# NOT USED
cc1 <-
            data.frame(Hyp="H1", Model="LS, SS", Var="Catch", rownames_to_column(as.data.frame(mod_h1[[1]]$data$Fcaton), var="Year") ) %>%
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", Var="Catch", rownames_to_column(as.data.frame(mod_h2[[1]]$data$Fcaton), var="Year")) ) %>%

  pivot_longer(names_to="Stock", values_to="Value", fishery1:fishery4) %>%
  mutate(Stock = ifelse(Hyp=="H1", "comb", Stock)) %>%
  mutate(Stock = ifelse(Hyp=="H2" & Stock == "fishery3", "north", Stock)) %>%
  mutate(Stock = ifelse(Hyp=="H2" & Stock %in% c("fishery1","fishery2","fishery4"), "south", Stock)) %>%
  group_by(Hyp, Model, Stock, Var, Year) %>%
  summarise(Value = sum(Value, na.rm=TRUE)) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(Var = factor(Var, levels=c("Catch","SSB (kt)","Fishing Mortality","Recruitment (numbers)")))  %>%
  mutate(Hyp = ifelse(Hyp=="H1", "H1 (one stock)", "H2 (two stock)"))  %>%
  mutate(Stock = ifelse(Stock=="comb","", Stock))


# NOT USED
cc2 <-
            data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Catch", Fmult="1.0", mod_h1[[1]]$output[[1]]$Catch_fut_1) %>%
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Catch", Fmult="0.75", mod_h1[[1]]$output[[1]]$Catch_fut_2)) %>%
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Catch", Fmult="1.25", mod_h1[[1]]$output[[1]]$Catch_fut_3))%>%
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Catch", Fmult="FMSY", mod_h1[[1]]$output[[1]]$Catch_fut_4))%>%
  bind_rows(data.frame(Hyp="H1", Model="LS, SS", stock="comb" ,Var="Catch", Fmult="0.0", mod_h1[[1]]$output[[1]]$Catch_fut_5))%>%

  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south" ,Var="Catch", Fmult="1.0", mod_h2[[1]]$output[[1]]$Catch_fut_1)) %>%
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south" ,Var="Catch", Fmult="0.75", mod_h2[[1]]$output[[1]]$Catch_fut_2)) %>%
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south" ,Var="Catch", Fmult="1.25", mod_h2[[1]]$output[[1]]$Catch_fut_3))%>%
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south" ,Var="Catch", Fmult="FMSY", mod_h2[[1]]$output[[1]]$Catch_fut_4))%>%
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="south" ,Var="Catch", Fmult="0.0", mod_h2[[1]]$output[[1]]$Catch_fut_5))%>%

  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north" ,Var="Catch", Fmult="1.0", mod_h2[[1]]$output[[2]]$Catch_fut_1)) %>%
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north" ,Var="Catch", Fmult="0.75", mod_h2[[1]]$output[[2]]$Catch_fut_2)) %>%
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north" ,Var="Catch", Fmult="1.25", mod_h2[[1]]$output[[2]]$Catch_fut_3))%>%
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north" ,Var="Catch", Fmult="FMSY", mod_h2[[1]]$output[[2]]$Catch_fut_4))%>%
  bind_rows(data.frame(Hyp="H2", Model="LS, SS", stock="north" ,Var="Catch", Fmult="0.0", mod_h2[[1]]$output[[2]]$Catch_fut_5))%>%


  setNames( c("Hyp", "Scenario","Stock","Var", "Fmult", "Year","Value")) %>%
  mutate(Year = as.numeric(Year)) %>%
  mutate(Var = factor(Var, levels=c("Catch","SSB (kt)","Fishing Mortality","Recruitment (numbers)")))  %>%
  mutate(Hyp = ifelse(Hyp=="H1", "H1 (one stock)", "H2 (two stock)"))  %>%
  mutate(Stock = ifelse(Stock=="comb","", Stock)) %>%
  mutate(fcgroup = paste0(Fmult, Stock))




# plot nominal
ep <-
  mdf %>%
  filter(Year >= 2000) %>%
  group_by(Hyp, Scenario, Stock, Var) %>%
  filter(Value == max(Value, na.rm=TRUE))


mdf %>%
  filter(Year >= 1974) %>%
  # View()

  ggplot(aes(x=Year, y=Value, group=Stock)) +
  theme_bw() +
  theme(legend.position = "none") +

  geom_line(aes(colour=Stock), size=1) +
  geom_ribbon(aes(fill=Stock, ymin=lb, ymax=ub), alpha=0.4) +
  ggrepel::geom_text_repel(data=ep,
                           aes(x=Year, y=Value, label = Stock, colour=Stock)) +

  geom_line(data=filter(rp,
                        Year>= 1974),
            aes(colour=Stock), linetype="dashed") +
  ggrepel::geom_text_repel(data=filter(rp,
                                       Year == 1974),
                           aes(x=Year, y=Value, label = Refpoint, colour=Stock)) +

  # geom_line(data=filter(cc1, Year >= 1974),
  #           aes(x=Year, y=Value, colour=Stock), inherit.aes = FALSE) +
  # geom_line(data=cc2, aes(x=Year, y=Value, colour=Stock, linetype=Fmult, group=fcgroup), inherit.aes = FALSE) +
  # geom_line(data=fc, aes(x=Year, y=Value, colour=Stock, linetype=Fmult, group=Fmult), inherit.aes = FALSE) +

  labs(y="", x="") +

  ggthemes::scale_color_colorblind() +
  ggthemes::scale_fill_colorblind() +

  facet_grid(Var ~ Hyp, scales="free_y")
