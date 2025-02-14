# helpers_ntw-compare.R
# 2025-01-23

# dumping help functions and things
# see 2024 entsoc files for some ideas/longevity stuff


# package loading ---------------------------------------------------------
library(tidyverse)
library(patchwork) # for plot_layout


# data loading ------------------------------------------------------------

# dev stats
d23 <- read.csv("~/Documents/repos/ntw/2023/data/clean-ntw.csv", header = TRUE) %>% mutate(year = 2023)
d24 <- read.csv("~/Documents/repos/ntw/2024/data/ntw.csv", header = TRUE) %>% mutate(year = 2024)

# longevity data
  # im dumb and survival data is in a separate file,
  # jdates were calculated using the year so the format is different from d23 lol,
  # hence a lot of convolution
source("../2023/helpers_tents.R")

d23_longevity <- data_longevity %>%
  filter(!is.na(jdate.died)) %>%
  select(id, sex, trt, jdate.ec, jdate.died) %>%
  rename(jdate.lec = jdate.ec,
         jdate.lsurv = jdate.died)

rm(data_tstats, data_hatch, data_longevity,
   labels.alltrts, labels.exptrts, RYB,
   x_err_ncoll, x_err_ncollf, y_err_hrate)


# function making ---------------------------------------------------------


## some useful generics ----------------------------------------------------

## calc se
se <- function(x){
  sd(na.omit(x))/sqrt(length(na.omit(x)))
}

## filters

filter.2pops <- function(data){
  data <- filter(data, !(diet == "TB" & diet == "lab"))
  
  return(data)
}

## calcing stats and then stitching it together ----------------------------

## stage-specific summary stats (need wide data)

### do calcs
calc.devstats <- function(data){
  
  # larval stats
  ss_la <- data %>%
    group_by(year, pop, trt.type, minT,
    ) %>%
    summarise(avg.tt = mean(na.omit(tt.pupa)),
              se.tt = se(tt.pupa),
              # avg.tt = mean(na.omit(jdate.pupa - jdate.hatch)),
              # se.tt = sd(na.omit(jdate.pupa - jdate.hatch))/sqrt(length(na.omit(jdate.pupa - jdate.hatch))),
              # n = n() - sum(na.omit(surv.outcome == 2)),
              # n.pmd = sum(na.omit(surv.outcome == 0)), 
              # n.surv = n - n.pmd, 
              # prop.survpup = round(1-(n.pmd/n), digits=2),
              n = n(),
              n.misc = sum(na.omit(surv.outcome == 2)), # things that died by accident
              n.dev = n - n.misc, 
              n.pmd = sum(na.omit(surv.outcome == 1)),
              n.pup = sum(na.omit(surv.outcome == 0)),
              prop.survpup = round(1 - (n.pup/n.dev), digits = 2)
    ) %>% 
    mutate(stage = "la")
  
  # pupal dev (eclosion time)
  ss_pu <- data %>%
    filter(!is.na(sex)) %>%
    group_by(year, pop, sex, trt.type, minT) %>%
    summarise(avg.tt = mean(na.omit(tt.eclose)),
              se.tt = se(tt.eclose),
              # avg.tt = mean(na.omit(jdate.eclose - jdate.pupa)),
              # se.tt = sd(na.omit(jdate.eclose - jdate.pupa))/sqrt(length(na.omit(jdate.eclose - jdate.pupa))),
              avg.mass = mean(na.omit(mass.pupa)),
              se.mass = sd(na.omit(mass.pupa)/sqrt(length(na.omit(mass.pupa)))),
              n = n()) %>%
    mutate(stage = "pu")
  
  # adult stats
  ss_ad.sex <- data %>%
    filter(!is.na(jdate.eclose)) %>%
    group_by(year, pop,
             trt.type, minT, sex
    ) %>%
    summarise(avg.tt = mean(na.omit(tt.surv)),
              se.tt = se(tt.surv),
              # avg.tt = mean(na.omit(jdate.surv - jdate.eclose)),
              # se.tt = sd(na.omit(jdate.surv - jdate.eclose))/sqrt(length(na.omit(jdate.surv - jdate.eclose))),
              avg.mass = mean(na.omit(mass.eclose)),
              se.mass = sd(na.omit(mass.eclose)/sqrt(length(na.omit(mass.eclose)))),
              n = sum(na.omit(!is.na(jdate.eclose)))
    ) %>%
    mutate(stage = "ad")
  
  ss_ad <- data %>%
    filter(!is.na(jdate.eclose)) %>%
    group_by(year, pop,
             trt.type, minT
    ) %>%
    summarise(avg.tt = mean(na.omit(tt.surv)),
              se.tt = se(tt.surv),
              # avg.tt = mean(na.omit(jdate.surv - jdate.eclose)),
              # se.tt = sd(na.omit(jdate.surv - jdate.eclose))/sqrt(length(na.omit(jdate.surv - jdate.eclose))),
              avg.mass = mean(na.omit(mass.eclose)),
              se.mass = sd(na.omit(mass.eclose)/sqrt(length(na.omit(mass.eclose)))),
              n = sum(na.omit(!is.na(jdate.eclose)))
    ) %>%
    mutate(stage = "ad",
           sex = "all")
  
  return(list(ss_la, ss_pu, ss_ad.sex, ss_ad))
  
}

### pull together
calc.ssadj <- function(data){
  data <- data %>%
    mutate(stage = factor(stage, levels = c("la", "pu", "ad")),
           avg.mass = avg.mass/1000,
           se.mass = se.mass/1000)
}



## more dev stats
  # 2025-02-13: maybe smush into another fn lol

calc.ssmoredev <- function(data){
  moredev <- data %>%
    mutate(dmass = (mass.pupa - mass.eclose)/1000,
           rate.pup = (mass.pupa/tt.pupa)/1000,
           rate.puplog = log(mass.pupa)/tt.pupa)
  
  ss_moredev <- moredev %>%
    group_by(year, pop, trt.type, minT) %>%
    summarise(n = n(),
              avg.dmass = mean(na.omit(dmass)),
              se.dmass = se(dmass),
              avg.ratepup = mean(na.omit(rate.pup)),
              se.ratepup = se(rate.pup),
              avg.ratepupl = mean(na.omit(rate.puplog)),
              se.ratepupl = se(rate.puplog)) %>%
    mutate(sex = "both") # "f+m"
  
  ss_moredev.sex<- moredev %>%
    filter(!is.na(sex)) %>%
    group_by(year, pop, sex, trt.type, minT) %>%
    summarise(n = n(),
              avg.dmass = mean(na.omit(dmass)),
              se.dmass = se(dmass),
              avg.ratepup = mean(na.omit(rate.pup)),
              se.ratepup = se(rate.pup),
              avg.ratepupl = mean(na.omit(rate.puplog)),
              se.ratepupl = se(rate.puplog))
  
  return(Reduce(full_join, (list(ss_moredev, ss_moredev.sex))))
  
}


# df prepping -------------------------------------------------------------

# things for modeling


  