# title: 2024 ntw analyses helpers
# date: 2024-09-23

# lifted from helpers_ox.R

# dependencies:
## outputs of `cleaning_ntw.Rmd`, but see below

# 0. package and data loading ---------------------------------------------

library(tidyverse)
library(conflicted)
conflicts_prefer(dplyr::filter)

wide_all <- read.csv("~/Documents/repos/ntw/2024/data/ntw.csv", header = TRUE)


# 1. wide manipulations ----------------------------------------------------

## add trt labels (idk revisit this when u actually need it lol)
wide_all <- wide_all %>%
  mutate(labs.trt = case_when(trt == 260 ~ "26±0°C",
                              trt == 419 ~ "40-19°C",
                              trt == 426 ~ "40-26°C",
                              trt == 433 ~ "40-33°C"))

# calculate instar lengths
wide_all <- wide_all %>%
  filter(instar.enter == "hatch") %>%
  select(-c("jdate.enter", "instar.enter")) %>%
  mutate(tt.3rd = jdate.3rd - jdate.hatch,
         tt.4th = jdate.4th - jdate.hatch,
         tt.5th = jdate.5th - jdate.hatch,
         tt.6th = jdate.6th - jdate.hatch,
         tt.7th = jdate.7th - jdate.hatch,
         tt.wander = jdate.wander - jdate.hatch,
         tt.pupa = jdate.pupa - jdate.hatch,
         tt.eclose = jdate.eclose - jdate.pupa,
         #tt.exit = jdate.exit - jdate.hatch, # time to dev outcome
         #tt.surv = jdate.surv - jdate.eclose, # time spent as adult
         )

# this needs to be jdate.enter if including those that entered later in the expt?
# wide_all <- wide_all %>%
#   filter(instar.enter %in% c("hatch", "2nd")) %>% # drops those super late entered things
#   mutate(tt.3rd = jdate.3rd - jdate.enter,
#          tt.4th = jdate.4th - jdate.enter,
#          tt.5th = jdate.5th - jdate.enter,
#          tt.6th = jdate.6th - jdate.enter,
#          tt.7th = jdate.7th - jdate.enter,
#          tt.wander = jdate.wander - jdate.enter,
#          tt.pupa = jdate.pupa - jdate.enter,
#          tt.eclose = jdate.eclose - jdate.pupa,
#          #tt.exit = jdate.exit - jdate.enter, # time to dev outcome
#          #tt.surv = jdate.surv - jdate.eclose, # time spent as adult
#          )



# 2. pivot to long --------------------------------------------------------

long_all <- wide_all %>%
  pivot_longer(cols = starts_with(c("jdate", "mass", "tt")),
               names_to = c(".value", "instar"),
               values_drop_na = TRUE,
               names_pattern = ("([a-z]*\\d*)\\.(\\d*[a-z]*)")) %>%
  drop_na(jdate) %>%
  filter(instar != "2nd") %>% # want to keep "hatch" for mathing
  drop_na(tt) # drops NA's if an individual didnt reach a certain stage



# 3. helper objects -------------------------------------------------------

# aesthetic things idk


