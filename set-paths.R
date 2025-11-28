# 2025-02-19
# title: "set-2024-paths.R"

# pathfinding with 'here'
# no "-" in the list name to prevent breaking!!!

here::i_am("set-paths.R")
library(here)

bin_paths <- list(y25 = list(root = here("2025"),
                             tdt = here("2025", "tdt"),
                             tdtdata = here("2025", "tdt", "data"),
                             ntw = here("2025", "ntw"),
                             ntwdata = here("2025", "ntw", "data")),
                  
                  y24 = list(root = here("2024"),
                             data = here("2024", "data"),
                             clean = here("2024", "01-cleaning"),
                             wrangle = here("2024", "02-wrangle"),
                             doviz = here("2024", "03-viz"),
                             dostats = here("2024", "03-stats")),
                  
                  y23 = list(root = here("2023"),
                             data = here("2023", "data")))

