# ntw 2025 utils
# 2025-11-28


# readme ------------------------------------------------------------------

# utility and user-defined functions for ntw wrangling/analyses/etc 
# cuz i always lose them in the sauce



# wrangle -----------------------------------------------------------------

## cleaning -----------------------------------------------------------------
StdiseColTypes <- function(data){
  data %>% 
    mutate(trt = as.numeric(trt),
           id = as.numeric(id),
           across(starts_with("jdate"), as.numeric))
}

# for sup, only cares abt things reaching at least 5th
CodeDevOutcomes <- function(data){
  data %>%
    mutate(fate.code = case_when(fate.dev %in% c("ec", "pup") ~ 1,
                                 fate.dev == "pmd" ~ 0,
                                 fate.dev == "other" ~ 2),
           sup = case_when(!is.na(jdate.8th) ~ 8,
                           !is.na(jdate.7th) ~ 7,
                           !is.na(jdate.6th) ~ 6,
                           !is.na(jdate.5th) ~ 5,
                           #FALSE ~ as.numeric(sup)
           )
    )
}