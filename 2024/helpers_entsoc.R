# helpers_entsoc.R
# 2024-10-22

# bc im tired of fucking around w the upstream stuff:
# copied/pasted sections of analyses_ntw-compare.Rmd focusing on the 2023 data



# load data and pkgs ------------------------------------------------------
library(tidyverse)
library(patchwork) # for plot_layout

d23 <- read.csv("~/Documents/repos/ntw/2023/data/clean-ntw.csv", header = TRUE) %>% mutate(year = 2023)


# df wrangling ------------------------------------------------------------

# filter NTW data and column renaming
d23 <- d23 %>%
  filter(reason.ignore != "lost" | src != "F1" #| !is.na(final.fate)
  ) %>% 
  filter(trt.stage %in% c("260-hatch", "337-hatch", "419-hatch","433-hatch")) %>%
  filter(expt.group %in% c("C", "D", "E", "F", "H")) %>%
  rename(trt = treatment,
         id = ID,
         cohort = expt.group,
         notes.ignore = reason.ignore)

# standardise values
d23$trt[d23$trt == 337] <- 426

d23 <- d23 %>%
  mutate(final.fate = case_when(notes.ignore %in% c("hot larva", "cut", "culled larva", "wet diet") ~ "other",
                                TRUE ~ as.character(final.fate)))

# simplify values
d23 <- d23 %>%
  mutate(sup = case_when(sup = 0 & !is.na(jdate.5th) ~ NA_real_,
                         TRUE ~ as.numeric(sup)),
         surv.outcome = case_when(!is.na(date.pupa) | !is.na(date.LP) ~ 0,
                                  final.fate == "pmd" ~ 1,
                                  final.fate == "other" ~ 2), # group injuries together
         flucT = case_when(flucT == 2.5 ~ 0,
                           TRUE ~ as.numeric(flucT)),
         trt.type = case_when(meanT == 26 & flucT == 0 ~ "ctrl",
                              TRUE ~ "expt"),
         trt = as.numeric(trt)
  )

### TROUBLESHOOTING
backup_d23.init <- d23

# pick relevant columns
d23 <- d23 %>%
  select(c("cohort", "pop", "diet", "trt", "id", # identifying info
           "instar.enter", starts_with(c("jdate", "mass")), 
           "sex",
           ends_with("T", ignore.case = FALSE), "trt.type",
           "sup", "surv.outcome"
  )) %>%
  select(-c("jdate.collected", "jdate.stuck", "jdate.LP", "jdate.died",
            ends_with("7th"), ends_with("15")))


# drop things that died in 1 day
d23 <- filter(d23, jdate.pmd - jdate.enter > 1 | is.na(jdate.pmd - jdate.enter))


# add exit dates and other convenience things for later
d23 <- d23 %>% 
mutate(jdate.exit = case_when(!is.na(jdate.pmd) ~ jdate.pmd,
                              !is.na(jdate.pupa) ~ jdate.pupa,
                              TRUE ~ NA_integer_),
       minT = factor(minT, levels = c(19, 26, 33)))


### TROUBLESHOOTING
backup_d23.all <- d23

# more heavy-handed filtering:
  # focus on lab bugs for entsoc

d23 <- filter(d23, pop == "lab" & diet == "LD")