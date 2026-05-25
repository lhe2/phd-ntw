# ntw mss analysis loading.R
# 2026-05-24

# purpose:
## for loading libs, data, utilities, etc collated viz/stats analysis
## for ntw mss.
## also sets factor levels, etc for plotting/stats



# libs ----------------------------------------------------------
library(tidyverse)

# viz
library(patchwork) # plot_layout

# stats
library(lme4) # glm, lm, glmer
#library(pscl) # zeroinfl
#library(ggfortify) # autoplot (diagnosing)


# utils ----------------------------------------------------------
library(here)

source(here("_ntw/scripts/wrangle-dev.R"))
source(here("_ntw/scripts/wrangle-tents.R"))

source(here("_ntw/scripts/analysis/filter_utils.R"))
source(here("_ntw/scripts/analysis/viz_utils.R"))
source(here("_ntw/scripts/analysis/stats_utils.R"))

# development data --------------------------------------------------------

## development data
source(here("_ntw/scripts/R/tidy-dev.R"))

# viz dfs: drop colony bugs + factorised for graphing
dfs_viz <- list(
  wide = dfs_tidy$wide,
  long = dfs_tidy$long
) %>%
  lapply(., \(df) {
    df %>%
      FilterForNTWBugs() %>% 
      filter(pop != "col") %>%
      mutate(trt.facet = case_when(trt == 260 ~ "ctrl",
                                   pop == "lab" ~ "lab",
                                   pop == "field" ~ "field")) %>%
      mutate(across(c(starts_with("trt"), "year"), as.factor))
  })




# egg data ----------------------------------------------------------------

## viz dfs: 
source(here("_ntw/scripts/R/tidy-tents.R"))

dfs_viz <- list_modify(
  dfs_viz,
  eggs = dfs_tidy$tents %>% # ss_nocol
    select(-mate.col) %>% 
    mutate(mate.pop = case_when(mate.pop == "col" ~ "lab",
                                TRUE ~ as.character(mate.pop))) %>%
    CalcTentCounts() %>%
    group_by(across(c("year", starts_with(c("mate", "trt"))))) %>%
    CalcTentSS() %>%
    FilterForLabEggs() %>%
    # make factors
    mutate(trt.minT = case_when(mate.trt == 419 ~ 19,
                                mate.trt == 433 ~ 33,
                                TRUE ~ 26),
           across(c("year", "mate.trt", starts_with("trt")), as.factor),
           mate.type = factor(mate.type, levels = c("within", "between", "virgin")),
           trt.mate = factor(trt.mate, levels = c("neither", "both", "f", "m"))
           )
)



  


# cleanup -----------------------------------------------------------------

rm(dfs_tidy)
