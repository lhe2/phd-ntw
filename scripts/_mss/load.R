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
library(ggfortify) # autoplot (diagnosing)
#library(pscl) # zeroinfl

conflicted::conflicts_prefer(
  dplyr::select(),
  dplyr::filter())

# utils ----------------------------------------------------------
library(here)

source(here("scripts/wrangle-dev.R"))
source(here("scripts/wrangle-tents.R"))

source(here("scripts/analysis/utils/filters.R"))
source(here("scripts/analysis/utils/viz.R"))
source(here("scripts/analysis/utils/stats.R"))

## export functions ----------------------------------------------------------

# function to save results (specify dev/ or tents/)
## dfs-* = from load.R
## N-* = other results
## anova/* = stats
## ss/* = viz
ResToCsv <- function(res, filename){
  res <- as.data.frame(res)
  today <- format(Sys.time(), "%y%m%d")
  path <- paste0("out/mss-stats/", filename, "_", today, ".csv")
  
  write.csv(res, here::here(path), row.names = TRUE)
}

# dont pass in/specify a plot object to save: just use
# default ggsave behaviour (saves most recently generated plot)
## specify if going to figs/supp/
ResToFig <- function(filename){
  #p <- gg
  fn <- paste0("~/Documents/PHD/_phd-outputs/mss-ntw/figs/", filename, ".png")
  
  ggsave(filename = here::here(fn), #plot = p, 
         dpi = "print")
}

# development data --------------------------------------------------------

## development data
source(here("scripts/analysis/load-dev.R"))

# viz dfs: drop colony bugs + factorised for graphing
dfs_viz <- list(
  wide = dfs_tidy$wide,
  long = dfs_tidy$long
) %>%
  lapply(., \(df) {
    df %>%
      filter(!(pop == "lab" & diet == "TB"),
             pop != "col") %>%
      mutate(across(c(starts_with("trt"), "year"), as.factor),
             pop = factor(pop, levels = c("lab", "field")))
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
      # FilterOutLabTB() %>%
      # FilterForNTWTrts() %>%
      filter(!(pop == "lab" & diet == "TB"),
             pop != "col",
             !is.na(is.pup), # drop culled bugs
      ) %>%
      # convert LPIs to 1's
      mutate(is.pup = case_when(!is.na(jdate.LPI) ~ 1,
                                TRUE ~ is.pup)) %>%
      # factorise and set reference levels
      mutate(across(c("year", "trt.minT", "trt.type", "trt"), as.factor),
             pop = factor(pop, levels = c("lab", "field")))
  })

# egg data ----------------------------------------------------------------

## viz dfs: 
source(here("scripts/R/tidy-tents.R"))

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
