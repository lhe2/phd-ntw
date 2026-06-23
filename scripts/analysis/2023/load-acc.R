# load 2023 dev data (accumulation expt)
# 2026-06-12

# load 2023 accumulation data into analysis scripts
# NOTE: dfs arent called "dfs_tidy" like in the ntw one...

source(here::here("scripts/R/tidy-dev.R"))

dfs_acc <- list(
  wide = df_wide %>%
    filter(year == 2023 & pop == "lab" &
             trt %in% c(260, 426)) %>%
    select(-parent.tent)
)

dfs_acc <- list_modify(
  dfs_acc,
  long = dfs_acc$wide %>% tolong()
)

rm(df_wide, tolong)