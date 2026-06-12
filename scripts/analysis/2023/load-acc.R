# load 2023 dev data (accumulation expt)
# 2026-06-12

# load 2023 accumulation data into analysis scripts
# NOTE: dfs arent called "dfs_tidy" like in the ntw one...

source(here::here("scripts/R/tidy-dev.R"))

dfs_acc <- list(
  wide = df_wide,
  long = df_long
) %>%
  lapply(., \(x){
    x %>%
      filter(year == 2023 & pop == "lab" &
               trt %in% c(260, 426)
      )
  })

rm(df_wide, df_long)