# title: helpers_tents.R
# date: 2024-02-20
# purpose: script of helper fns/code for ntw tent data

# based off helpers_ntw

# 0. package and data loading ------------------------------------------------

# basic data processing & viz
library(tidyverse)
library(survival)
library(survminer)
#library(gridExtra)

# cleaned data
data_tstats <- read.csv("~/Documents/repos/_private/data/ntw_data/clean-tentstats.csv")
data_tpairs <- read.csv("~/Documents/repos/_private/data/ntw_data/clean-tentpairs.csv")
data_hatch <- read.csv("~/Documents/repos/_private/data/ntw_data/clean-hatchstats.csv")
data_ntw <- read.csv("~/Documents/repos/_private/data/ntw_data/clean-ntw.csv")

# aesthetic objects
RYB <- c("#324DA0", "#acd2bb", "#f1c363", "#a51122")

# 1. generate adult longevity stats ---------------------------------------

# bc i spread this info out everywhere -.-

# merging adult info

# f_ntw <- data_ntw %>%
#   filter(sex == "f") %>%
#   select(c(ID, date.hatch, date.pupa, mass.pupa, expt.group, pop, diet, meanT, flucT, maxT, minT))
# 
# m_ntw <- data_ntw %>%
#   filter(sex == "m") %>%
#   select(c(ID, date.hatch, date.pupa, mass.pupa, expt.group, pop, diet, meanT, flucT, maxT, minT))

ntwadults <- data_ntw %>%
  filter(!(expt.group == "A" | expt.group == "B")) %>%
  select(c(ID, date.hatch, date.pupa, mass.pupa, expt.group, pop, diet, meanT, flucT, maxT, minT)) %>%
  mutate(ID = as.character(ID))

finfo <- data_tpairs %>%
  select(c(1:6, "id.f", "trt.f", starts_with("date.f"), pop, tent.loc, track.f)) %>%
  mutate(sex = "f", .after = "id.f")

minfo <- data_tpairs %>%
  select(c(1:6, "id.m", "trt.m", starts_with("date.m"), pop, tent.loc, track.m)) %>%
  mutate(sex = "m", .after = "id.m")

colnames(finfo) <- gsub("\\.f", "", colnames(finfo))
colnames(minfo) <- gsub("\\.m", "", colnames(minfo))

tentadults <- 
  bind_rows(finfo, minfo) %>%
  rename(pair.pop = pop,
         track.reason = track,
         pair.trt = trt.pair)

data_longevity <- full_join(tentadults, ntwadults, by = c("id" = "ID")) %>%
  drop_na(id.tent) %>%
  drop_na(id) %>%
  drop_na(date.hatch)



# reorder columns for sensibleness
data_longevity <- data_longevity[, c(4:5, 2, 13, 3, 6:9, 15:17, 11, 10, 18:24, 1, 12, 14)] 

# 3. format columns ----------------------------------------------------------

# list to iterate over
tenthelpers <- list(data_tstats, data_longevity, data_hatch)

# define formatting fn
mod.dates <- function(data) {
  data <- data %>%
    mutate(across(starts_with("date"), as.Date, format = "%Y-%m-%d"),
           across(starts_with("date"), as.Date, format, "%j", .names = "j{.col}"),
           across(starts_with("jdate"), as.numeric)
           # #date = as.Date(data_tstats$date, format = "%Y-%m-%d"),
           # #jdate = as.numeric(as.Date(, format = "%j"))
           )
}

# format data
tenthelpers_clean <- lapply(tenthelpers, mod.dates)
data_tstats <- tenthelpers_clean[[1]]
data_longevity <- tenthelpers_clean[[2]]
data_hatch <- tenthelpers_clean[[3]]


# add trt levels
data_tstats$trt.f <- factor(data_tstats$trt.f, levels = c(260, 419, 426, 433, 900))
data_tstats$trt.m <- factor(data_tstats$trt.m, levels = c(260, 419, 426, 433))

data_longevity$trt <- factor(data_longevity$trt, levels = c(260, 419, 426, 433))

# 4. cleanup --------------------------------------------------------------

rm(ntwadults, tentadults, finfo, minfo, 
   data_ntw, data_tpairs, 
   tenthelpers_clean, 
   mod.dates)

rm(tenthelpers)
