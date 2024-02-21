# title: helpers_tents.R
# date: 2024-02-20
# purpose: script of helper fns/code for ntw tent data

# based off helpers_ntw

# 0. package and data loading ------------------------------------------------

# basic data processing & viz
library(tidyverse)
library(gridExtra)

# cleaned data
data_tstats <- read.csv("~/Documents/repos/_private/data/ntw_data/clean-tentstats.csv")
data_tpairs <- read.csv("~/Documents/repos/_private/data/ntw_data/clean-tentpairs.csv")
data_ntw <- read.csv("~/Documents/repos/_private/data/ntw_data/clean-ntw.csv")




# merging adult info

f_ntw <- data_ntw %>%
  filter(sex == "f") %>%
  select(c(ID, date.hatch, date.pupa, mass.pupa, expt.group, pop, diet, meanT, flucT, maxT, minT))

m_ntw <- data_ntw %>%
  filter(sex == "m") %>%
  select(c(ID, date.hatch, date.pupa, mass.pupa, expt.group, pop, diet, meanT, flucT, maxT, minT))

ntwadults <- data_ntw %>%
  filter(!(expt.group == "A" | expt.group == "B")) %>%
  select(c(ID, date.hatch, date.pupa, mass.pupa, expt.group, pop, diet, meanT, flucT, maxT, minT)) %>%
  mutate(ID = as.character(ID))

finfo <- data_tpairs %>%
  select(c(1:6, "id.f", "trt.f", starts_with("date.f"), pop, track.f)) %>%
  mutate(sex = "f", .after = "id.f")

minfo <- data_tpairs %>%
  select(c(1:6, "id.m", "trt.m", starts_with("date.m"), pop, track.m)) %>%
  mutate(sex = "m", .after = "id.m")

colnames(finfo) <- gsub("\\.f", "", colnames(finfo))
colnames(minfo) <- gsub("\\.m", "", colnames(minfo))

test_pairs <- 
  bind_rows(finfo, minfo) %>%
  rename(pop.pair = pop,
         track.reason = track)

test_full <- full_join(test_pairs, ntwadults, by = c("id" = "ID")) %>%
  drop_na(id.tent) %>%
  drop_na(id) %>%
  drop_na(date.hatch)


# format julian dates
data_tstats <- data_tstats %>%
  mutate(date = as.Date(data_tstats$date, format = "%Y-%m-%d"),
         jdate = as.numeric(as.Date(data_tstats$date, format = "%j")))
