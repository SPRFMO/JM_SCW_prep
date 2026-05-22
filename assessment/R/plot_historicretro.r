# -----------------------------------------------------------------------------------------------
# CJM Historic retro plot
#
# 27/09/2017 first version
# 15/09/2018 SC06 2018. Now includes historical plot of reference points as well.
# 09/10/2019 SC07 2019. Updated during SC meeting
# 25/10/2020 SC08 2020. Updated for new Github file system and annex plots
# -----------------------------------------------------------------------------------------------

library(tidyverse)
library(cowplot)
library(directlabels)  # for printing labels at end of geom lines
library(scales)
library(readxl)

# lowcase function
lowcase <- function(df) {
  names(df) <- tolower(names(df)) %>% gsub("\\?|\\s+|\\.+|_+|\\(|\\)","",.)
  df
}

if(!"plotCount" %in% ls())
  plotCount <- 0

# load the data
d <-
  read.csv(file.path("hist_retro", "SPRFMO historical retro.csv"), header=TRUE) %>%
  lowcase() %>%
  filter(!(assessmenttype %in% c("benchmark", "mod1.4"))) %>%
  mutate(assessmenttype = ifelse(assessmentyear == max(assessmentyear),"last","assess"),
         tyear          = substr(as.character(assessmentyear),3,4)) %>%
  rename(f = favg) %>%
  data.frame()

# d %>% distinct(assessmentyear, assessmenttype) %>% View()


# plot ssb
p1 <-
  d %>%
  filter(!is.na(ssb)) %>%
  ggplot(aes(year,ssb, group=assessmentyear)) +
  theme_jjm() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        legend.position = "null") +
  geom_ribbon(aes(ymin = ssblow, ymax = ssbupp, fill = assessmenttype), alpha=0.2 ) +
  scale_fill_manual (values=c(last   = PFA_BLUE, assess = "white")) +
  geom_line(aes(colour = assessmenttype, linewidth=assessmenttype, linetype=assessmenttype) ) +
  geom_dl(aes(label  = tyear, colour=assessmenttype),
          method = list(dl.combine("last.points"), cex = 0.8)) +
  scale_colour_manual(values=c(last   = PFA_BLUE, assess = "grey40")) +
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  scale_linewidth_manual(values=c(last   = 1.5,
                                  assess = 0.8,
                                  bench  = 1.2,
                                  old    = 0.8)) +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "SSB ('000 t)")


# plot f
p2 <-
  d %>%
  filter(!is.na(f)) %>%
  ggplot(aes(year,f, group=tyear)) +
  theme_jjm() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        legend.position = "null") +
  geom_line(aes(colour = assessmenttype, linewidth=assessmenttype, linetype=assessmenttype) ) +
  geom_dl(aes(label  = tyear, colour = assessmenttype),
          method = list(dl.combine("last.points"), cex = 0.8)) +
  scale_colour_manual(values=c(last   = PFA_BLUE,
                               assess = "grey40",
                               bench  = PFA_BLUE2,
                               old    = "grey60")) +
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  scale_linewidth_manual(values=c(last   = 1.5,
                                  assess = 0.8,
                                  bench  = 1.2,
                                  old    = 0.8)) +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "F")

# plot recruitment
p3 <-
  d %>%
  filter(!is.na(r)) %>%
  ggplot(aes(year,r, group=tyear)) +
  theme_jjm() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        legend.position = "null") +
  geom_ribbon(aes(ymin = rlow, ymax = rupp, fill = assessmenttype), alpha=0.2 ) +
  scale_fill_manual (values=c(last   = PFA_BLUE, assess = "white")) +
  geom_line(aes(colour = assessmenttype, linewidth=assessmenttype, linetype=assessmenttype) ) +
  geom_dl(aes(label  = tyear, colour = assessmenttype),
          method = list(dl.combine("last.points"), cex = 0.8)) +
  scale_colour_manual(values=c(last   = PFA_BLUE,
                               assess = "grey40",
                               bench  = PFA_BLUE2,
                               old    = "grey60")) +
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  scale_linewidth_manual(values=c(last   = 1.5,
                                  assess = 0.8,
                                  bench  = 1.2,
                                  old    = 0.8)) +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "Recruitment (age 1; millions)")


# show plot
plot_grid(p1 + theme(legend.position = "none", axis.title = element_blank()),
          p2 + theme(legend.position = "none", axis.title = element_blank()),
          p3 + theme(legend.position = "none", axis.title = element_blank()),
          ncol=1, align = 'h', rel_widths = c(3,3,3))

# generate pdf
pg <- plot_grid(p1 + theme(legend.position = "none", axis.title = element_blank()),
          p2 + theme(legend.position = "none", axis.title = element_blank()),
          p3 + theme(legend.position = "none", axis.title = element_blank()),
          ncol=1, align = 'h', rel_widths = c(3,3,3))


# Extra plots ---------------------------------------------------------------------

# plot b over bmsy
p4 <-
  d %>%
  filter(!is.na(bbmsy)) %>%
  ggplot(aes(year,bbmsy, group=assessmentyear)) +
  theme_jjm() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        legend.position = "null") +
  geom_line(aes(colour = assessmenttype, linewidth=assessmenttype, linetype=assessmenttype) ) +
  geom_dl(aes(label  = tyear, colour=assessmenttype),
          method = list(dl.combine("last.points"), cex = 0.8)) +
  scale_colour_manual(values=c(last   = PFA_BLUE, assess = "grey40")) +
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  scale_linewidth_manual(values=c(last   = 1.5,
                                  assess = 0.8,
                                  bench  = 1.2,
                                  old    = 0.8)) +
  geom_hline(aes(yintercept=1), colour=PFA_BLUE) +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "B/Bmsy")

# plot f over fmsy
p5 <-
  d %>%
  filter(!is.na(ffmsy)) %>%
  ggplot(aes(year,ffmsy, group=assessmentyear)) +
  theme_jjm() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        legend.position = "null") +
  geom_line(aes(colour = assessmenttype, linewidth=assessmenttype, linetype=assessmenttype) ) +
  geom_dl(aes(label  = tyear, colour=assessmenttype),
          method = list(dl.combine("last.points"), cex = 0.8)) +
  scale_colour_manual(values=c(last   = PFA_BLUE, assess = "grey40")) +
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  scale_linewidth_manual(values=c(last   = 1.5,
                                  assess = 0.8,
                                  bench  = 1.2,
                                  old    = 0.8)) +
  expand_limits(y = 0) +
  geom_hline(aes(yintercept=1), colour=PFA_BLUE) +
  labs(x = NULL, y = NULL , title = "F/Fmsy")


# plot bmsy
p6 <-
  d %>%
  filter(!is.na(bmsy)) %>%
  ggplot(aes(year,bmsy, group=assessmentyear)) +
  theme_jjm() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        legend.position = "null") +
  geom_line(aes(colour = assessmenttype, linewidth=assessmenttype, linetype=assessmenttype) ) +
  geom_dl(aes(label  = tyear, colour=assessmenttype),
          method = list(dl.combine("last.points"), cex = 0.8)) +
  scale_colour_manual(values=c(last   = PFA_BLUE, assess = "grey40")) +
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  scale_linewidth_manual(values=c(last   = 1.5,
                                  assess = 0.8,
                                  bench  = 1.2,
                                  old    = 0.8)) +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "Bmsy")


# plot fmsy
p7 <-
  d %>%
  filter(!is.na(fmsy)) %>%
  ggplot(aes(year,fmsy, group=assessmentyear)) +
  theme_jjm() +
  theme(legend.title=element_blank(),
        axis.text.x = element_text(angle = 0, vjust = 0.5, size=9),
        axis.text.y = element_text(size=9),
        legend.position = "null") +
  geom_line(aes(colour = assessmenttype, linewidth=assessmenttype, linetype=assessmenttype) ) +
  geom_dl(aes(label  = tyear, colour=assessmenttype),
          method = list(dl.combine("last.points"), cex = 0.8)) +
  scale_colour_manual(values=c(last   = PFA_BLUE, assess = "grey40")) +
  scale_linetype_manual(values=c(last   = "solid",
                                 assess = "solid",
                                 bench  = "dashed",
                                 old    = "dotdash")) +
  scale_linewidth_manual(values=c(last   = 1.5,
                                  assess = 0.8,
                                  bench  = 1.2,
                                  old    = 0.8)) +
  expand_limits(y = 0) +
  labs(x = NULL, y = NULL , title = "Fmsy")

# show plot
plot_grid(p4 + theme(legend.position = "none", axis.title = element_blank()),
          p5 + theme(legend.position = "none", axis.title = element_blank()),
          p6 + theme(legend.position = "none", axis.title = element_blank()),
          p7 + theme(legend.position = "none", axis.title = element_blank()),
          ncol=2, align = 'hv', rel_widths = c(3,3), rel_heights = c(3,3))


pg2 <- plot_grid(p4 + theme(legend.position = "none", axis.title = element_blank()),
          p5 + theme(legend.position = "none", axis.title = element_blank()),
          p6 + theme(legend.position = "none", axis.title = element_blank()),
          p7 + theme(legend.position = "none", axis.title = element_blank()),
          ncol=2, align = 'hv', rel_widths = c(3,3), rel_heights = c(3,3))
