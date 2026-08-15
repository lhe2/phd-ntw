# load 2023 dev data (2x2 expt)
# 2026-06-12

# load 2023 2x2 data into analysis scripts
# NOTE: dfs arent called "dfs_tidy" like in the ntw one...

source(here::here("scripts/tidy/R/dev.R"))

dfs_temps <- list(
  wide = df_wide %>%
    filter(year == 2023 & pop == "lab" & 
             instar.enter == "hatch" &
             trt %in% c(260, 267, 330, 426)
    ) %>%
    select(-c(instar.enter, parent.tent))
)


dfs_temps <- list_modify(
  dfs_temps,
  long = dfs_tidy$wide %>% ToLong()
)


rm(df_wide)