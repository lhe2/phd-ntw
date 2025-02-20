## ----about-------------------------------------------------------------------------------------
# wrangle/ntw-compare.R

# knitted wrangling code for ntw comparison figs/analyses.
# source() this in corresponding analysis scripts.


## ----load utils--------------------------------------------------------------------------------
library(tidyverse)

here::i_am("2024/02-wrangle/ntw-compare.Rmd")
library(here)

source(here::here("set-paths.R"))


## ----load 2324 dev data------------------------------------------------------------------------
d23 <- read.csv(here(bin_paths23$data, "clean-ntw.csv"), header = TRUE) %>% mutate(year = 2023)
d24 <- read.csv(here(bin_paths24$data, "ntw.csv"), header = TRUE) %>% mutate(year = 2024)


## ----load 23 longevity data--------------------------------------------------------------------
source(here::here(bin_paths23$y23, "helpers_tents.R"))

d23_longevity <- data_longevity %>%
  filter(!is.na(jdate.died)) %>%
  select(id, sex, trt, jdate.ec, jdate.died) %>%
  rename(jdate.lec = jdate.ec,
         jdate.lsurv = jdate.died)

rm(data_tstats, data_hatch, data_longevity,
   labels.alltrts, labels.exptrts, RYB,
   x_err_ncoll, x_err_ncollf, y_err_hrate)


## ----match 2023 to 2024------------------------------------------------------------------------
# pick relevant data and rename columns/values to match 2024

# troubleshooting: compare column names
# setdiff(names(d24), names(d23)) # differences
# setdiff(names(d23), names(d24))
# intersect(names(d24), names(d23)) # same

# filter rows and rename columns
d23 <- d23 %>%
  filter(reason.ignore != "lost" | src != "F1" #| !is.na(final.fate)
           ) %>% 
  filter(trt.stage %in% c("260-hatch", "267-hatch", "330-hatch", "337-hatch", "419-hatch","433-hatch")) %>%
  filter(expt.group %in% c("A", "B", "C", "D", "E", "F", "H")) %>%
  rename(trt = treatment,
         id = ID,
         cohort = expt.group,
         notes.ignore = reason.ignore)

# standardise values
d23$trt[d23$trt == 337] <- 426

# d23 <- d23 %>%
#   mutate(final.fate = case_when(notes.ignore %in% c("hot larva", "cut", "culled larva", "wet diet") | final.fate == "culled" ~ "other",
#                                 TRUE ~ as.character(final.fate))
#          )

d23 <- d23 %>%
  mutate(fate = case_when(notes.ignore %in% c("culled larva", "cut", "hot larva", "lost", "wet diet") ~ "other"))

# match values to d24
d23 <- d23 %>%
  mutate(sup = case_when(!is.na(jdate.7th) ~ 7,
                         !is.na(jdate.6th) ~ 6,
                         is.na(jdate.pmd) & !is.na(jdate.pupa) ~ 0),
         # case_when(sup = 0 & !is.na(jdate.5th) ~ NA_real_,
         #                 TRUE ~ as.numeric(sup)),
         surv.outcome = case_when(fate == "other" | (is.na(jdate.pmd) & is.na(jdate.pupa)) ~ 2,
                                  !is.na(date.pupa) | !is.na(date.LP) ~ 1,
                                  !is.na(date.pmd) ~ 0,
                                  #TRUE ~ 2
                                  ),
         # surv.outcome = case_when(!is.na(date.pupa) | !is.na(date.LP) ~ 0,
         #                          final.fate == "pmd" ~ 1,
         #                          final.fate == "other" ~ 2), # treat injuries the same as this
         flucT = case_when(flucT == 2.5 ~ 0,
                           TRUE ~ as.numeric(flucT)),
         trt.type = case_when(meanT == 26 & flucT == 0 ~ "ctrl",
                              TRUE ~ "expt"),
         trt = as.numeric(trt),
         #id = paste(id, year, sep = "-")
         )

# troubleshooting
# filter(d23, is.na(surv.outcome)) %>% View()
#test %>% group_by(final.fate) %>% summarise(n = n())
#test %>% group_by(reason.ignore) %>% summarise(n = n())
#filter(test, is.na(final.fate)) %>% View()

test2 <- d23
#d23<-test2

# how to handle LPIs? (in 23 and 24?)

d23 <- d23 %>%
  select(-c("jdate.collected", "jdate.15", "jdate.stuck",
            "jdate.exit", "jdate.LP", "jdate.died")) %>% # d23 exclusives
  select(c("cohort", "pop", "diet", "trt", "id", # identifying info
           "instar.enter", starts_with(c("jdate", "mass")), 
           "sex",
           ends_with("T", ignore.case = FALSE), "trt.type",
           "sup", "surv.outcome", "year"
          ))

# append longevity data to d23
d23_longevity[d23_longevity$id == 1415, "sex"] <- "f" # fix data

d23 <- merge(d23, d23_longevity, all = TRUE)


## ----fine-tune 2024----------------------------------------------------------------------------
d24 <- d24 %>%
  #mutate(id = paste(id, year, sep = "-")) %>%
  select(-"jdate.culled") %>%
  filter(instar.enter == "hatch")


## ----final df----------------------------------------------------------------------------------
all <- merge(d24, d23, all = TRUE
             ) %>%
  drop_na(id) %>%
  select(-c("instar.enter",
            ))

# drop things that died in 1 day
  # 2025-01-23: can i do this??????
all <- filter(all, jdate.pmd - jdate.enter > 1 | is.na(jdate.pmd - jdate.enter))

# add in exit dates to more easily parse out things still developing (for 2024 data)
# and other convenience things
all <- all %>% 
  mutate(
        # jdate.exit = case_when(!is.na(jdate.pmd) ~ jdate.pmd,
        #                         !is.na(jdate.pupa) ~ jdate.pupa,
        #                         TRUE ~ NA_integer_),
         minT = factor(minT, levels = c(19, 26, 33)),
         year = factor(year, levels = c(2023, 2024)),
         is.sup = case_when(sup == 0 ~ 0,
                            sup > 1 ~ 1),
         for.ntw = case_when(#year == 2023 & cohort %in% c("A", "B") ~ "N",
                             trt %in% c(260, 419, 426, 433) ~ "Y",
                             TRUE ~ "N"),
         for.const = case_when(#flucT %in% c(0, 7) & meanT %in% c(26, 33) ~ "Y",
                               trt %in% c(260, 267, 330, 426) ~ "Y",
                               TRUE ~ "N"),
                               # trt.type == "ctrl" | (meanT == 33 & flucT %in% c(0, 7)) ~ "Y",
                               # TRUE ~ "N")
         )



## ----------------------------------------------------------------------------------------------
all_wide <- all %>%
  filter(!(pop == "lab" & diet == "TB")) %>%
  mutate(tt.3rd = jdate.3rd - jdate.hatch,
         tt.4th = jdate.4th - jdate.hatch,
         tt.5th = jdate.5th - jdate.hatch,
         tt.6th = jdate.6th - jdate.hatch,
         tt.7th = jdate.7th - jdate.hatch,
         tt.8th = jdate.8th - jdate.hatch, # omit if only d23
         tt.wander = jdate.wander - jdate.hatch,
         tt.pupa = jdate.pupa - jdate.hatch,
         tt.eclose = jdate.eclose - jdate.pupa,
         tt.surv = jdate.surv - jdate.eclose,
         tt.lsurv = jdate.lsurv - jdate.lec, # for d23 longevity
         tt.pmd = jdate.pmd - jdate.hatch
         )  %>%
  # drop extra d23 longevity columns
  mutate(tt.surv = case_when(is.na(tt.surv) ~ as.numeric(tt.lsurv),
                             TRUE ~ as.numeric(tt.surv))) %>%
  #filter(year == 2023 & sex == "f" & !is.na(jdate.lsurv)) %>% View()
  select(-c("jdate.lsurv", "jdate.lec", "tt.lsurv"))

all_long <- all_wide %>%
  pivot_longer(cols = starts_with(c("jdate", "mass", "tt")),
               names_to = c(".value", "instar"),
               values_drop_na = TRUE,
               names_pattern = ("([a-z]*\\d*)\\.(\\d*[a-z]*)")) %>%
  drop_na(jdate) %>%
  drop_na(tt) # drops NA's if an individual didnt reach a certain stage


# filtering by expt type
ntw_wide <- filter(all_wide, for.ntw == "Y")
#cvf_wide <- filter(all_wide, for.const == "Y")


## ----------------------------------------------------------------------------------------------
# apply calcs to data
ss_all <- lapply(list(ntw_wide, cvf_wide), calc.devstats)

ss_ntw <- Reduce(full_join, ss_all[[1]]) %>% 
  calc.ssadj()

# ss_cvf <- Reduce(full_join, ss_all[[2]]) %>%
#   calc.ssadj()


## ----------------------------------------------------------------------------------------------
ss_moredev <- ntw_wide %>%
  filter(!(diet == "TB" & pop == "lab")) %>% 
  calc.ssmoredev()


## ----------------------------------------------------------------------------------------------
# pmds + sup of those that pup'ed
ss_devall <- ntw_wide %>%
  mutate(trt = as.factor(trt)) %>%
  filter(!(diet == "TB" & pop == "lab")) %>%
  group_by(year, diet, pop, trt) %>%
  summarise(n.tot = n(),
            n.pmd = sum(na.omit(surv.outcome == 0)),
            n.misc = sum(na.omit(surv.outcome == 2)),
            n.pup = sum(na.omit(surv.outcome == 1)),
            n.sup = sum(na.omit(surv.outcome == 1 & sup > 0)),
            n.dev = n.tot - n.misc, # = pmd + pups of any sup
            n.5th = sum(na.omit(sup == 0 & surv.outcome == 1)),
            n.6th = sum(na.omit(sup == 6 & surv.outcome == 1)),
            n.7th = sum(na.omit(sup == 7 & surv.outcome == 1)),
            n.8th = sum(na.omit(sup == 8 & surv.outcome == 1)),
            p.pmd = round(n.pmd/n.dev, digits = 3),
            p.sup = round(n.sup/n.dev, digits = 3),
            p.5th = round(n.5th/n.dev, digits = 3),
            p.6th = round(n.6th/n.dev, digits = 2),
            p.7th = round(n.7th/n.dev, digits = 2),
            p.8th = round(n.8th/n.dev, digits = 2)
            )


# sup of those that pmd'd
ss_devpmd <- ntw_wide %>%
  mutate(trt = as.factor(trt)) %>%
  filter(!(diet == "TB" & pop == "lab")) %>%
  filter(surv.outcome == 1) %>%
  group_by(year, diet, pop, trt) %>%
  summarise(n.tot = n(),
            n.5th = sum(na.omit(sup == 0)),
            n.6th = sum(na.omit(sup == 6)),
            n.7th = sum(na.omit(sup == 7)),
            n.8th = sum(na.omit(sup == 8)),
            p.5th = round(n.5th/n.tot, digits = 2),
            p.6th = round(n.6th/n.tot, digits = 2),
            p.7th = round(n.7th/n.tot, digits = 2),
            p.8th = round(n.8th/n.tot, digits = 2)
            )

# pivots
ss_devall2 <- ss_devall %>%
  pivot_longer(cols = starts_with(c("n.", "p.")),
               names_to = c(".value", "stage"),
               names_pattern = ("([a-z]*)\\.(\\d*[a-z]*)")) %>%
  filter(stage %in% c("pmd", "5th", "6th", "7th"))

ss_devpmd2 <- ss_devpmd %>%
  pivot_longer(cols = starts_with(c("n.", "p.")),
               names_to = c(".value", "stage"),
               names_pattern = ("([a-z]*)\\.(\\d*[a-z]*)")) %>%
    filter(stage %in% c("5th", "6th", "7th"))


## ----general ntw filter------------------------------------------------------------------------
ntw_expt <- ntw_wide %>%
  filter(!(diet == "TB" & pop == "lab") & trt.type == "expt") %>%
  mutate(dmass = mass.pupa - mass.eclose,
         rate.pup = mass.pupa/tt.pupa)


## ----specific ntw filters----------------------------------------------------------------------
# omit accidental deaths (so either pmd, pup, LPI)
ntw_expt.surv <- ntw_expt %>%
  filter(surv.outcome != 2) 

# omit unsexed pupa
ntw_expt.sex <- ntw_expt %>%
  filter(!is.na(sex))

