# load ntw dev data
# 2026-06-12

# filter out ntw-only dev data and load into analysis scripts
# (bc i did a lot of stuff in 2023)

source(here::here("scripts/R/tidy-dev.R"))

dfs_tidy <- list(
  wide = df_wide,
  long = df_long
  ) %>%
  lapply(., \(x){
    x %>%
      filter(year == 2023 & (instar.enter == "hatch" |
                               pop == "col" |
                               (pop == "field" & instar.enter %in% c("1st", "2nd"))) |
               year %in% c(2024, 2025),
             trt %in% c(260, 419, 426, 433)
      ) %>%
      select(-instar.enter)
  })

rm(df_wide, df_long)