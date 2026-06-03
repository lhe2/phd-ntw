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

conflicted::conflicts_prefer(
  dplyr::select(),
  dplyr::filter())

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
      mutate(across(c(starts_with("trt"), "year"), as.factor)
      )
  })

# stats dfs: subset into ctrl+ntw bugs and just ntw bugs (omit col bugs)
## focus on pop, minT
dfs_stats <- list(
  # TODO maybe include just a ctrl bug subset...
  dev_all = dfs_tidy$wide,
  dev_expt = dfs_tidy$wide %>% filter(trt != 260)
) %>%
  lapply(., \(x) {
    x %>%
      FilterOutLabTB() %>%
      FilterForNTWTrts() %>%
      filter(pop != "col",
             is.pup < 2, # drop culled bugs
             ) %>%
      # factorise and set reference levels
      mutate(across(c("year", "trt.minT", "trt.type", "trt"), as.factor),
             pop = factor(pop, levels = c("lab", "field")))
  })



# egg data ----------------------------------------------------------------

## viz dfs: 
source(here("_ntw/scripts/R/tidy-tents.R"))

dfs_viz <- list_modify(
  dfs_viz,
  eggs = dfs_tidy$tents %>% # ss_nocol
    filter(trt.pop != "col") %>% # drop colony-only tents
    select(-mate.col) %>% 
    mutate(mate.pop = case_when(mate.pop == "col" ~ "lab",
                                TRUE ~ as.character(mate.pop))) %>%
    CalcTentCounts() %>%
    group_by(across(c("year", starts_with(c("mate", "trt"))))) %>%
    CalcTentSS(),
  
  eggs_noyr = dfs_tidy$tents %>% # ss_nocol
    filter(trt.pop != "col") %>% # drop colony-only tents
    select(-mate.col) %>% 
    mutate(mate.pop = case_when(mate.pop == "col" ~ "lab",
                                TRUE ~ as.character(mate.pop))) %>%
    CalcTentCounts() %>%
    group_by(across(c(starts_with(c("mate", "trt"))))) %>%
    CalcTentSS() %>%
    mutate(year = NA)
)

dfs_viz[c("eggs", "eggs_noyr")] <- dfs_viz[c("eggs", "eggs_noyr")] %>%
  lapply(.,\(x){
    x %>%
      FilterForLabEggs() %>% # drops 2024 stuff here
      # make factors
      mutate(trt.minT = case_when(mate.trt == 419 ~ 19,
                                  mate.trt == 433 ~ 33,
                                  TRUE ~ 26),
             across(c("year", "mate.trt", starts_with("trt")), as.factor),
             mate.type = factor(mate.type, levels = c("within", "between", "virgin")),
             trt.mate = factor(trt.mate, levels = c("neither", "both", "f", "m"))
             
  )}
  )




  


# cleanup -----------------------------------------------------------------

rm(dfs_tidy)
