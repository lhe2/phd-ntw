# title: 2024 analyses helpers
# date: 2024-07-16
# purpose: does some pre-loading stuff for analysis scripts

# this worked well for 2023 lol so we'll do it again

# dependencies:
  # outputs of `cleaning.Rmd`

# 0. package and data loading ---------------------------------------------

library(tidyverse)
conflicted::conflicts_prefer(dplyr::filter)

wide_all <- read.csv("~/Documents/repos/ntw/2024/data/cleaned-data.csv", header = TRUE)


# 1. wide calculations ----------------------------------------------------

wide_all <- wide_all %>%
  mutate(tt.3rd = jdate.3rd - jdate.hatch,
         tt.4th = jdate.4th - jdate.hatch,
         tt.5th = jdate.5th - jdate.hatch,
         tt.wander = jdate.wander - jdate.hatch, 
         tt.died = jdate.pmd - jdate.hatch,
         tt.intrt = case_when(is.na(jdate.pmd) ~ jdate.pupa - jdate.3rd,
                              TRUE ~ jdate.pmd - jdate.3rd),
         is.pmd = case_when(is.na(jdate.pmd) ~ 0,
                            TRUE ~ 1) # pmd stuff gets dropped in the pivot regardless
         )

# 2. pivot to long, formatting --------------------------------------------

# stats of major dev pts
long_major <- wide_all %>%
  pivot_longer(cols = starts_with(c("jdate", "mass", "tt")),
               names_to = c(".value", "instar"),
               values_drop_na = TRUE,
               names_pattern = ("([a-z]*)\\.(\\d*[a-z]*)")) #%>% # 2024-07-18 TOFIX: drops the pmd things early
  #drop_na(jdate) %>% drop_na(tt) # drops NA's if an individual didnt reach a certain stage

#### RUN UP TO HERE UNTIL THINGS R FIXED ####

# fine-scale stats for 4th and 5th only
  # this calculates the day-by-day weight changes in the 4th and the 5th instars
long_fine <- wide_all %>%
  select(c("trt", "id", "jdate.4th", starts_with("mass.4"), "jdate.5th", starts_with("mass.5"))) %>%
  rename(mass.4d0 = mass.4th, mass.5d0 = mass.5th) %>%
  pivot_longer(cols = starts_with("jdate"),
               names_to = c(".value", "instar"), names_sep = "\\.", 
               values_drop_na = TRUE) %>%
  ### does all instars together ###
  # pivot_longer(cols = starts_with("mass"),
  #              #names_to = c(".value", "d4th"), names_sep = "\\."
  #              names_to = "dx", names_prefix = "(mass\\.)(\\d)(d)", names_transform = as.integer,
  #              values_to = "mass", values_drop_na = TRUE) %>%
  # mutate(jdate_d = jdate + dx) %>%
  # select(-jdate) %>%
  # rename(jdate = jdate_d)
  ### does 4th and 5th separately ###
  pivot_longer(cols = starts_with("mass.4"),
               #names_to = c(".value", "d4th"), names_sep = "\\."
               names_to = "d4th", names_prefix = "mass.4d", names_transform = as.integer,
               values_to = "mass4th", values_drop_na = TRUE) %>%
  #mutate(keep = case_when(instar == "4th" ~ "keep_4", TRUE ~ "drop_5")) %>%
  #filter(keep == "yes") %>%
  pivot_longer(cols = starts_with("mass.5"),
              #names_to = c(".value", "d4th"), names_sep = "\\."
              names_to = "d5th", names_prefix = "mass.5d", names_transform = as.integer,
              values_to = "mass5th", values_drop_na = TRUE) %>%
  mutate(jdate_d = case_when(instar == "4th" ~ jdate + d4th,
                             instar == "5th" ~ jdate + d5th))

# other way to pivot sth like "mass.4d1"
  # pivot_longer(cols = starts_with(c("jdate", "mass")),
  #              names_to = c(".value", "instar"),
  #              values_drop_na = TRUE,
  #              names_pattern = ("([a-z]*)\\.(\\d*[a-z]*\\d*)"))


# merge the fine-scale back onto the overall long df
long_all <- merge(long_major, long_fine, all = TRUE)
long_all <- rows_patch(long_major, long_fine, by = "ID")
  

# 3. long calculations ----------------------------------------------------


# helper functions --------------------------------------------------------

## adding integer breaks to a plot
  # from: https://stackoverflow.com/a/62321155/17952236
  # ok this is not what i want LOL (n will determine the # of breaks u get)

integer_breaks <- function(n, ...) {
  fxn <- function(x) {
    breaks <- floor(pretty(x, n = n, ...))
    names(breaks) <- attr(breaks, "labels")
    unique(breaks)
  }
  return(fxn)
}
