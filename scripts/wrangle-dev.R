# ntw wrangle dev fns
# 2026-03-12

## making ntw wrangle more like tdt wrangle.
## just the wrangle/summary fns are here -- the filtering/grouping is in analysis-viz

## load math fns
source(here("scripts/math_utils.R"))


# dev calcs -----------------------------------------------------------------
## df prep
  # establish some "default" filters and groups for larval and adult dev,
  # with some extra manipulations depending on the life stage.

PrepLarvalSS <- function(long_df){
  long_df %>%
    filter(pop != "col",
           !is.na(diet), # some random bugs...
           !instar %in% c("eclose", "long"
                          #"exit", # NOTE breaks groups. too many unique exit dates
                          #"fridge", # NOTE omit for now. not that useful
                          )) %>%
    group_by(across(c(year, pop, diet, starts_with("trt"), instar))) # default groupings
}

PrepAdultSS <- function(long_df){
  long_df %>%
    filter(instar %in% c("pupa", "eclose", "long"
                         #"fridge"
                         )) %>%
    group_by(across(c(year, pop, diet, starts_with("trt"), instar))) %>%
    mutate(mass = mass/1000)
}

# development summary stats
CalcDevSS <- function(long_df){
  long_df %>%
    mutate(logmass = log(mass),
           #mass_g = mass/1000, # only want to do this for adults tho lol..
           devrate = 1/tt) %>%
    summarise(n = n(),
              across(.cols = c(mass, logmass, #mass_g, 
                               tt, devrate),
                     .fns = list(avg = ~ mean(.x, na.rm = TRUE),
                                 se = se),
                     .names = "{.fn}.{.col}")) %>%
    ungroup()
}

# combined fns
## uses "default" groupings
CalcDevSS_ad <- function(long_df){
  long_df %>%
    PrepAdultSS() %>%
    CalcDevSS()
}

CalcDevSS_la <- function(long_df){
  long_df %>%
    PrepLarvalSS() %>%
    CalcDevSS()
}


# proportions calcs --------------------------------------------------------------------

## survival proportions
PrepSurvProps <- function(wide_df){
  wide_df %>% 
    filter(is.pup != 2, # drop culled
           pop != "col",
           !is.na(diet)) %>% 
    group_by(across(c(year, pop, diet, starts_with("trt"))))
}

CalcSurvProps <- function(wide_df){
  wide_df %>%
    summarise(n = n(),
              prop.pup = sum(is.pup == 1)/n,
              se.pup = seprop(prop.pup, n)) %>%
    ungroup()
}

## combined fn
CalcSurvProps_def <- function(wide_df){
  wide_df %>%
    PrepSurvProps() %>%
    CalcSurvProps()
}


## supernumerary proportions
PrepSupProps <- function(wide_df){
  wide_df %>%
    filter(is.pup != 2, # drop culled
           pop != "col",
           !is.na(diet)) %>% 
    group_by(across(c(year, pop, diet, starts_with("trt"))))
}

CalcSupProps <- function(wide_df){
  wide_df %>%
    summarise(N = n(),
              surv_tot = sum(is.pup == 1),
              surv_0th = sum(is.pup == 1 & is.na(sup)),
              surv_6th = sum(is.pup == 1 & sup == 6, na.rm = TRUE),
              surv_7th = sum(is.pup == 1 & sup == 7, na.rm = TRUE),
              surv_8th = sum(is.pup == 1 & sup == 8, na.rm = TRUE),
              died_tot = sum(is.pup == 0),
              died_0th = sum(is.pup == 0 & is.na(sup)),
              died_6th = sum(is.pup == 0 & sup == 6, na.rm = TRUE),
              died_7th = sum(is.pup == 0 & sup == 7, na.rm = TRUE),
              died_8th = sum(is.pup == 0 & sup == 8, na.rm = TRUE),
    ) %>%
    pivot_longer(cols = starts_with(c("surv", "died")),
                 names_to = c("status", ".value"),
                 names_sep = "_") %>%
    mutate(not = N - tot,
           across(c("not", "0th", "6th", "7th", "8th"), ~ ./N,
                  .names = "p_{.col}")) %>%
    rename_with(~ paste0("n_", .x), matches("^(not)|^(\\dth)")) %>%
    pivot_longer(cols = starts_with(c("n_", "p_")),
                 names_to = c(".value", "sup"), names_sep = "_") %>%
    mutate(sup = gsub(sup, pattern = "th", replacement = ""),
           sup = case_when(sup == "not" & status == "surv" ~ "died",
                           sup == "not" & status == "died" ~ "survived",
                           TRUE ~ as.character(sup))
           ) %>%
    ungroup()
}

# combined fn
CalcSupProps_def <- function(wide_df){
  wide_df %>%
    PrepSupProps() %>%
    CalcSupProps()
}