# 2025-02-19
# title: "set-2024-paths.R"

# pathfinding with 'here'

here::i_am("set-paths.R")
library(here)

bin_paths25 <- list(y25 = here("2025"),
                    data = here("2025", "data"))

bin_paths24 <- list(y24 = here("2024"),
                    data = here("2024", "data"),
                    clean = here("2024", "01-cleaning"),
                    wrangle = here("2024", "02-wrangle"),
                    doviz = here("2024", "03-viz"),
                    dostats = here("2024", "03-stats"))

bin_paths23 <- list(y23 = here("2023"),
                    data = here("2023", "data"))

