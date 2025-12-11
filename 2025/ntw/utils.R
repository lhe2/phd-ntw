# ntw 2025 utils
# 2025-11-28


# readme ------------------------------------------------------------------

# utility and user-defined functions for ntw wrangling/analyses/etc 
# cuz i always lose them in the sauce

# functions -----------------------------------------------------------------
### MATH ###
se <- function(x){ 
  sd(na.omit(x))/sqrt(length(na.omit(x)))
}

se.prop <- function(p, n){
  sqrt((p*(1-p))/n)
}

### PLOTTING ###
# sets error bar width/height to some % of the other axis
## struggling w ggplot error bar width issues again... 
## see also https://gist.github.com/tomhopper/9076152 for suggested defaults

CalcErrWd <- function(x, pct = 0.12){
  max(x) * pct 
}

CalcErrHt <- function(y, pct = 0.07){
  max(y) * pct
}

### CONVENIENCE ###
FilterOutLabTB <- function(data){
  data %>%
    filter(!(pop == "lab" & diet == "TB"))
}

FilterForLabEggs <- function(data){
  data %>%
    filter(mate.pop != "field",
           mate.type != "virgin f") 
}


# GroupNTW <- function(data){
#   data %>%
#     group_by(year, pop, diet, trt)
# }




##### dont need a lot of this stuff ooops cuz i changed things
# cleaning -----------------------------------------------------------------

# moved out into the script itself 

# StdiseColTypes <- function(data){
#   data %>% 
#     mutate(trt = as.numeric(trt),
#            id = as.numeric(id),
#            across(starts_with("jdate"), as.numeric))
# }
# 
# # for sup, only cares abt things reaching at least 5th
# CodeDevOutcomes <- function(data){
#   data %>%
#     mutate(fate.code = case_when(fate.dev %in% c("ec", "pup") ~ 1,
#                                  fate.dev == "pmd" ~ 0,
#                                  fate.dev == "other" ~ 2),
#            sup = case_when(!is.na(jdate.8th) ~ 8,
#                            !is.na(jdate.7th) ~ 7,
#                            !is.na(jdate.6th) ~ 6,
#                            !is.na(jdate.5th) ~ 5,
#                            #FALSE ~ as.numeric(sup)
#            )
#     )
# }

# wrangle -----------------------------------------------------------------
# dont need anymore bc combined everything lol


# TODO include exit date calcs if culled, survived thru adult, pmd, etc? 

# .CalcTT <- function(long_data){
#   long_data %>%
#     mutate(tt.3rd = jdate.3rd - jdate.hatch,
#            tt.4th = jdate.4th - jdate.hatch,
#            tt.5th = jdate.5th - jdate.hatch,
#            tt.6th = jdate.6th - jdate.hatch,
#            tt.7th = jdate.7th - jdate.hatch,
#            tt.8th = jdate.8th - jdate.hatch,
#            tt.wander = jdate.wander - jdate.hatch,
#            tt.pupa = jdate.pupa - jdate.hatch,
#            tt.eclose = jdate.eclose - jdate.hatch,
#     )
# }

# .PivotLongerDev <- function(wide_data){
#   wide_data %>%
#     pivot_longer(cols = starts_with(c("jdate", "mass", "tt")),
#                  names_to = c(".value", "instar"),
#                  values_drop_na = TRUE,
#                  names_pattern = ("([a-z]*\\d*)\\.(\\d*[a-z]*)")) %>%
#     drop_na(jdate) %>%
#     drop_na(tt) # drops NAs if individual didn't reach a certain stage
# }

