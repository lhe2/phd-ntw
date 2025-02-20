# 2025-02-19
# title: "set-2024-paths.R"

# pathfinding with 'here'

here::i_am("set-2024-paths.R")
library(here)

bin_paths <- list(y24 = here("2024"),
                  data = here("2024", "data"),
                  bin = here("2024", "00-bin"),
                  util = here("2024", "00-util"),
                  clean = here("2024", "01-cleaning"),
                  wrangle = here("2024", "02-wrangle"),
                  do = here("2024", "03-analysis"))



