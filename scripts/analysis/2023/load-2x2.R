# load 2023 dev data (2x2 expt)
# 2026-06-12

# load 2023 2x2 data into analysis scripts
# NOTE: dfs arent called "dfs_tidy" like in the ntw one...

source(here::here("scripts/R/tidy-dev.R"))

dfs_temps <- list(
  wide = df_wide,
  long = df_long
) %>%
  lapply(., \(x){
    x %>%
      filter(year == 2023 & pop == "lab" & 
               instar.enter == "hatch" &
               trt %in% c(260, 267, 330, 426)
      ) %>%
      select(-instar.enter)
  })

rm(df_wide, df_long)