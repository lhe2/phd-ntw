# title: helpers_tents.R
# date: 2025-01-14
# purpose: helper tent fns for ntw tent data 2024

# based off `2023/helpers_tents.R`


# 0. package & data loading -----------------------------------------------

# basic data processing & viz
library(tidyverse)
#library(survival)
#library(survminer)
#library(gridExtra)

library(lme4)
library(lmerTest)

# cleaned data
data_longevity <- read.csv("~/Documents/repos/ntw/2024/data/clean-longevity.csv")
data_fertility <- read.csv("~/Documents/repos/ntw/2024/data/clean-fertility.csv")


# 1. column formatting ----------------------------------------------------

tentdata <- list(data_longevity, data_fertility)

mod.dates <- function(data) {
  data <- data %>%
    mutate(across(starts_with("date"), as.Date, format = "%Y-%m-%d"),
           across(starts_with("date"), as.Date, format, "%j", .names = "j{.col}"),
           across(starts_with("jdate"), as.numeric),
           minT = case_when((trt == 260 | trt == 426) ~ 26,
                            trt == 419 ~ 19,
                            trt == 433 ~ 33),
           expt.type = case_when(trt == 260 ~ "ctrl",
                                 TRUE ~ "expt")
    )
}

tentdata_clean <- lapply(tentdata, mod.dates)
data_longevity <- tentdata_clean[[1]]
data_fertility <- tentdata_clean[[2]]


# 2. cleanup --------------------------------------------------------------

rm(mod.dates,
   tentdata, tentdata_clean)
