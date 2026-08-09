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
library(lme4) # lmer, glmer
library(lmerTest) # overload lme4::lmer to get p-vals on fixed effs 
  ## drop1/anova(type=2) for lmms
library(car) # type II, III ANOVAs


conflicted::conflicts_prefer(
  dplyr::select(),
  dplyr::filter(),
  lmerTest::lmer() # to make things of class lmerModLmerTest
  )

# utils ----------------------------------------------------------
library(here)

source(here("scripts/wrangle-dev.R"))
source(here("scripts/wrangle-tents.R"))

source(here("scripts/analysis/utils/filters.R"))
source(here("scripts/analysis/utils/viz.R"))
source(here("scripts/analysis/utils/stats.R"))

## legend functions --------------------------------------------------------

# development plots (pop vs trt.type by minT)
Plot_Dev <- function(df){
  df %>%
    ggplot(data = df %>% arrange(desc(trt)), # plot ctrl over NTW if overplotted
           mapping = aes(x = trt.minT,
                         shape = pop,
                         lty = pop,
                         col = trt.type,
                         group = interaction(pop, trt.type)
           )) +
    labs(lty = "population", shape = "population",
         col = "maximum larval\ntemperature",
         x = p_scales$xlab_minT
    ) +
    scale_color_manual(values = p_scales$cols_trttype,  
                       labels = p_scales$labs_trttype) +
    scale_shape_manual(values = p_scales$shp_pop) +
    scale_linetype_manual(values = p_scales$lty_pop) +
    scale_x_discrete(labels = p_scales$labs_minT) +
    guides(color = guide_legend(override.aes = list(alpha = 1, size = 2),
                                order = 1),
           shape = guide_legend(override.aes = list(alpha = 1, size = 2,
                                                    linetype = c("solid", "255F")),
                                order = 2),
           lty = guide_legend(order = 2),
           size = guide_legend(override.aes = list(alpha = 0.65),
                               order = 3)
           ) +
    theme_bw()
}

# fitness plots (trted sex vs minT)
Plot_Fit <- function(df){
  df %>%
    ggplot(aes(x = trt.minT,
               lty = trt.mate, 
               shape = trt.mate,
               group = trt.mate,
               col = trt.mate)) +
    labs(col = "heat-treated sex", lty = "heat-treated sex", shape = "heat-treated sex",
         x = p_scales$xlab_minT) +
    scale_shape_manual(values = p_scales$shp_trtsex, labels = p_scales$labs_trtmate) +
    scale_color_manual(values = p_scales$cols_trtsex, labels = p_scales$labs_trtmate) +
    scale_linetype_manual(values = p_scales$lty_trtsex, labels = p_scales$labs_trtmate) +
    scale_x_discrete(labels = c(p_scales$labs_trt)) +
    guides(shape = guide_legend(override.aes = list(linetype = c("blank", "solid",
                                                                 "3223", "4224")))) +
    theme_bw()
}

## export functions ----------------------------------------------------------

# function to save results (specify dev/ or tents/)
## dfs-* = from load.R
## N-* = other results
## anova/* = stats
## ss/* = viz
ResToCsv <- function(res, filename){
  res <- as.data.frame(res) 
  # TODO use broom::tidy()...?
  # res <- tidy(res)
  today <- format(Sys.time(), "%y%m%d")
  path <- paste0("out/mss-stats/", filename, "_", today, ".csv")
  
  write.csv(res, here::here(path), row.names = TRUE)
}

# dont pass in/specify a plot object to save: just use
# default ggsave behaviour (saves most recently generated plot)
## specify if going to figs/supp/
ResToFig <- function(filename){
  #p <- gg
  fn <- paste0("~/Documents/PHD/_phd/docs/mss-ntw/figs/", filename, ".png")
  
  ggsave(filename = here::here(fn), #plot = p, 
         dpi = "print")
}

# development data --------------------------------------------------------

## development data
source(here("scripts/analysis/load-dev.R"))

# dfs_tidy <- dfs_tidy %>%
#   lapply(., \(x){
#     x %>%
#       filter(!(year == 2023 & cohort %in% c("A", "B")))
#   })

# viz dfs: drop colony bugs + factorise for graphing
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
  eggs_n = dfs_tidy$tents %>% 
    # pre-filter pops
    filter(trt.pop != "col", # drop colony-only tents
           mate.pop != "field", mate.type != "virgin" # drop field, 2024 stuff, unmated tents
    ) %>% 
    # remove colony-mating stuff
    ## (not that relevant if all bugs are lab)
    # mutate(mate.pop = case_when(mate.pop == "col" ~ "lab",
    #                             TRUE ~ as.character(mate.pop))) %>%
    select(-c(mate.col, trt.pop, mate.pop)) %>%
    CalcTentCounts()
)

dfs_viz <- list_modify(
  dfs_viz,
  eggs_ss = dfs_viz$eggs_n %>%
    group_by(across(c("year", starts_with(c("mate", "trt"))))) %>%
    CalcTentSS()
    )


# make factors
dfs_viz[c("eggs_n", "eggs_ss")] <- dfs_viz[c("eggs_n", "eggs_ss")] %>%
  lapply(., \(x){
    x %>%
    mutate(trt.minT = case_when(mate.trt == 419 ~ 19,
                                mate.trt == 433 ~ 33,
                                TRUE ~ 26),
           across(c("year", "mate.trt", starts_with("trt")), as.factor),
           mate.type = factor(mate.type, levels = c("within", "between")),
           trt.mate = factor(trt.mate, levels = c("neither", "both", "f", "m"))
    )
  })
    


# stats dfs
dfs_stats <- list_modify(
  dfs_stats,
  eggs_all = dfs_tidy$tents,
  eggs_expt = dfs_tidy$tents %>%
    filter(mate.trt != 260)
)

dfs_stats[c("eggs_all", "eggs_expt")] <- dfs_stats[c("eggs_all", "eggs_expt")] %>%
  lapply(., \(x){
    x %>%
      filter(mate.pop != "field", mate.type != "virgin", # drop 2024 stuff
             trt.pop != "col", # drop colony-only tents
             ) %>% 
      select(-c(mate.col, mate.pop, 
                trt.pop # not needed once everything is lab
                )) %>% 
      CalcTentCounts() %>%
      mutate(trt.minT = case_when(mate.trt == 419 ~ 19,
                                  mate.trt == 433 ~ 33,
                                  TRUE ~ 26),
             across(c("year", starts_with(c("trt", "mate"))), as.factor),
             # relevel for stats comparisons
             mate.ishs = factor(mate.ishs, levels = c("none", "both", "f", "m")),
             mate.trt = recode_factor(mate.trt, `260` = "26", `419` = "19", `426` = "26", `433` = "33")
      )
  })

# cleanup -----------------------------------------------------------------

rm(dfs_tidy)
