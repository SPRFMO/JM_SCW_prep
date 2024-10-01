#- Script to take the jjm assessment
# use adnuts to generate uncertainty and 
# forward iterations under different F / TAC regimes
# 
# Copyright Iago MOSQUEIRA (WMR), 2020-24
# Author: Iago MOSQUEIRA (WMR) <iago.mosqueira@wur.nl>, Niels Hintzen <nhintzen@pelagicfish.eu.
#
# Distributed under the terms of the EUPL-1.2


library(icesTAF)

library(FLjjm)
library(mse)
library(adnuts)

# SETUP
setwd("C:/Repositories/SPRFMO/jjm/assessment/")
mkdir("forecast/basecase")
cp("input/*", "forecast/basecase")
cp("config/*", "forecast/basecase")
cp("C:/Repositories/jmOM/utilities.R", "forecast")
source("forecast/utilities.R")

os <- "windows" # windows, osx
exe <- "jjms" # jjms.exe

# SETUP adnuts
iters <- 500
chains <- 5
burnin <- 1/4
thin <- 10

iter <- ((iters * thin) * (1 + burnin)) / chains
seeds <- round(runif(chains, 122, 869))


# --- SINGLE stock
model <- "h1_1.00"
path <- "forecast/basecase/"

# SIMPLE run: jjms -nox -ind h1_1.07.ctl -iprint 100
dir <- exejjms(model, path = path, clean=FALSE)

dir <- exejjms(model, path = path, clean=FALSE,
  args="-binp jjms.bar -phase 22 -hbf")

# adnuts EXPECTS ./jjms, ADD ln -s
file.copy(system.file('bin', os, exe, package='FLjjm'),
  file.path(path, exe))

# RUN adnuts (56 min, R 4.3.2, Ubuntu 22.04, 5 cores, 32 Gb)
system.time(
mcout <- sample_nuts(model="jjms", path=path,
  iter=iter, warmup=iter/5, seeds=seeds, chains=chains, cores=chains,
  admb_args=paste0("-ind ", model, ".ctl"), mceval=TRUE,
  control=list(metric="mle", adapt_delta=0.9, max_treedepth=10, refresh=1))
)

#- Get mceval out as adnuts leaves the file empty
setwd(path)
system(paste("jjms -ind", paste0(model, ".ctl -mceval")))
setwd("C:/Repositories/SPRFMO/jjm/assessment/")

# SAVE
save(mcout, file=paste0("forecast/basecase/mcout_", model, ".rda"), compress="xz")


# Generate FLR OM object from the output
its <- 500
fy  <- 2045

om <- readFLomjjm(model, path = "forecast/basecase",
  projection=mseCtrl(method=fwd.om), iter=its)

# EXTEND to future
om <- fwdWindow(om, end=fy, deviances=ar1deviances, nsq=3)

# SAVE
save(om, file=file.path(path,"h1_1.00.rda"), compress="xz")

#- Setup statistics and generation of different scenarios
#- get statistics
data(statistics, package="FLjjm")

# ADD SB statistic
statistics$SB <- list(~SB, name="SSB", desc="")

# ARGS
yrs <- seq(2025, 2045)

# --- h1_1.00 LT projections
load("output/h1_1.00.rda")
h1_runs <- list()

# Need to define the ratio in catch in future years
#  Assume average over the past 3 years
cbfis <- yearMeans(abind(lapply(fisheries(om), function(y)
  catch(y[[1]])[, ac(2021:2023),,,,1])))

ratio <- c(cbfis/max(cbfis))

# FWD for F = F_2024
f2024 <- fbar(om)[[1]][,'2024']

fwctrl <- controlAllocate(f2024, yrs, quant="fbar",
  allocation=ratio)

h1_runs$f2024 <- fwd(om, control=fwctrl)

# FWD for F = 0

fwctrl <- controlAllocate(-1, yrs, quant="fbar", allocation=ratio)

h1_runs$f0 <- fwd(om, control=fwctrl)

# FWD for F = FMSY

fwctrl <- controlAllocate(refpts(om)$FMSY, yrs, quant="fbar", allocation=ratio)

h1_runs$fmsy <- fwd(om, control=fwctrl)

# FWD for F2024 * 0.75

fwctrl <- controlAllocate(F2024 * 0.75, yrs, quant="fbar",
  allocation=ratio)

h1_runs$F2024075 <- fwd(om, control=fwctrl)

# FWD for F2024 * 1.25

fwctrl <- controlAllocate(F2024 * 1.25, yrs, quant="fbar",
  allocation=ratio)

h1_runs$F2024125 <- fwd(om, control=fwctrl)

# FWD for TAC2024 (1,242 kt)

tac2024 <- 1242

fwctrl <- controlAllocate(tac2024, yrs, quant="fbar",
  allocation=ratio)

h1_runs$tac2024 <- fwd(om, control=fwctrl)

# FWD for TAC2024 * 0.75

fwctrl <- controlAllocate(tac2024 * 0.75, yrs, quant="fbar",
  allocation=ratio)

h1_runs$tac2024075 <- fwd(om, control=fwctrl)

# FWD for TAC2024 * 1.25

fwctrl <- controlAllocate(tac2024 * 1.25, yrs, quant="fbar",
  allocation=ratio)

h1_runs$tac2024125 <- fwd(om, control=fwctrl)

# FWD for C = [500, 1500, length=26]

library(future.apply)
plan(multicore, workers=2)

cas <- seq(500, 1500, length=26)

h1_runs <- c(h1_runs,
future_lapply(setNames(cas, nm=paste0("C", cas)), function(x) {
    fwctrl <- controlAllocate(x, yrs, quant="catch", allocation=ratio)
  fwd(om, control=fwctrl)
  })
)

# SET output as FLmses
h1_runs <- FLmses(lapply(h1_runs, window, start=2024))

performance(h1_runs) <- performance(h1_runs,
  metrics=list(C=catch, SB=ssb, F=fbar),
  statistics=statistics[c("C", "SB", "PSBMSY")],
  years=c(2025, 2026, 2030, 2034))

# SAVE
save(h1_runs, file="forecast_h1_1.00.rda", compress="xz")


# -- write results into a TABLE

per <- performance(h1_runs)

# SELECT year & statistic, COMPUTE summary (median | percentage)
lis <- list(
  # C2025
  C2025=per[year == 2025 & statistic == "C",
    .('Catch (2025)'=median(data)), by=mp],
  # C2026
  C2026=per[year == 2026 & statistic == "C",
    .('Catch (2026)'=median(data)), by=mp][,2],
  # B2026
  B2026=per[year == 2026 & statistic == "SB",
    .('B[2026]'=median(data)), by=mp][,2],
  # P(B2026>BMSY)
  PB2026BMSY=per[year == 2026 & statistic == "PSBMSY",
    .('P(B[2025]>B[MSY] %'=sum(data) / 500 * 100), by=mp][,2],
  # B2030
  B2030=per[year == 2030 & statistic == "SB",
    .('B[2030]'=median(data)), by=mp][,2],
  # P(B2030>BMSY)
  PB2030BMSY=per[year == 2030 & statistic == "PSBMSY",
    .('P(B[2025]>B[MSY] %'=sum(data) / 500 * 100), by=mp][,2],
  # B2034
  B2034=per[year == 2034 & statistic == "SB",
    .('B[2034]'=median(data)), by=mp][,2],
  # P(B2034>BMSY)
  PB2034BMSY=per[year == 2034 & statistic == "PSBMSY",
    .('P(B[2025]>B[MSY] %'=sum(data) / 500 * 100), by=mp][,2]
)

tab <- do.call(cbind, lis)
colnames(tab) <- unlist(lapply(lis, colnames))

fwrite(tab, file="forecast_table.csv")
