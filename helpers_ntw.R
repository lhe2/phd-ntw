# title: ntw_helper-functions.R
# date: 2024-01-08
# purpose: script of custom helper functions for ntw analyses

# note that the data cleaning script still needs to be run separately 
# BEFORE this script, if any changes get made to the gsheets!

# this script should work for general packages, etc stuff needed in the ntw scripts
# i could make a package but ngl idk how to do that yet SO!



# 0. package and data loading ------------------------------------------------

# basic data processing & viz
#library(conflicted)
library(tidyverse)
#conflicts_prefer(dplyr::filter)
library(gridExtra)

# survival stats/viz
library(survival)
library(survminer)

# cleaned data
#wide_all <- read.csv("~/Documents/repos/_private/data/ntw_data/clean-gsheets.csv", header = TRUE)
wide_all <- read.csv("~/Documents/repos/_private/data/ntw_data/clean-ntw.csv", header = TRUE)

# 1. calculations & pivoting ---------------------------------------------------

# calculate instar lengths
wide_all <- wide_all %>% select(-(starts_with("date."))) %>%
  mutate(tt.3rd = jdate.3rd - jdate.hatch,
         tt.4th = jdate.4th - jdate.hatch,
         tt.5th = jdate.5th - jdate.hatch,
         tt.6th = jdate.6th - jdate.hatch,
         tt.7th = jdate.7th - jdate.hatch, 
         tt.wander = jdate.wander - jdate.hatch,
         #tt.pupa = jdate.pupa - jdate.wander,
         tt.pupa = jdate.pupa-jdate.hatch,
         tt.15 = jdate.15 - jdate.hatch,
         tt.eclose = jdate.eclose - jdate.pupa,
         tt.exit = jdate.exit - jdate.hatch, # time to dev outcome
         tt.surv = jdate.surv - jdate.eclose, # time spent as adult
         tt.trt = jdate.exit - jdate.enter) # time spent in temp trt

# pivot
long_all <- wide_all %>%
  pivot_longer(cols = starts_with(c("jdate", "mass", "h", "tt")),
               names_to = c(".value", "instar"),
               #names_sep = ".",
               values_drop_na = TRUE,
               names_pattern = ("([a-z]*)\\.(\\d*[a-z]*)")) %>%
  rename(molt.status = h) %>%
  drop_na(jdate) %>% drop_na(tt) %>% # drops NA's if an individual didnt reach a certain stage
  filter(instar != "15")

# add instar factor levels
long_all$instar <- factor(long_all$instar, levels=c("hatch", "2nd", "3rd", "4th", "5th", "6th", "7th", "stuck", "wander", "15", "pupa", "eclose", "exit", "trt", "surv"))
#long_all$instar <- factor(long_all$instar, c("hatch", "2nd", "3rd", "4th", "5th", "6th", "7th", "stuck", "wander", "15", "pupa", "eclose", "exit"))


# 2. define helper functions & objects ------------------------------------------

# grouping functions ######

### for temps
filter.temps2 <- function(data) {
  filtered_data <- data %>% 
    filter(trt.stage == "260-hatch" | trt.stage =="267-hatch" | trt.stage == "330-hatch" | trt.stage == "337-hatch") %>%
    filter(expt.group == "A" | expt.group == "B")
  
  return(filtered_data)
}


### for acc
filter.acc2 <- function(data) {
  filtered_data <- data %>% 
    filter(trt.stage != "267-hatch" & trt.stage != "330-hatch" & trt.stage != "419-hatch" & trt.stage != "433-hatch") %>% mutate(trt.stage = factor(trt.stage, levels = c("260-hatch", "337-hatch", "337-3rd", "337-4th"))) %>%
    filter(expt.group == "B")
  
  return(filtered_data)
}


### for NTs
filter.NTs2 <-function(data){
  filtered_data <- data %>% 
    filter(trt.stage == "260-hatch" | trt.stage == "337-hatch" | trt.stage == "419-hatch" | trt.stage == "433-hatch") %>% 
    mutate(trt.stage = factor(trt.stage, levels = c("260-hatch", "419-hatch", "337-hatch", "433-hatch"))) %>%
    filter(expt.group == "C" | expt.group == "D" | expt.group == "E" | expt.group == "F" | expt.group == "H")
  
  return(filtered_data)
}

### for fertility (compare kids to parents)
filter.F1s <- function(data) {
  filtered_data <- data %>% 
    filter(trt.stage == "260-hatch" | trt.stage == "337-hatch" | trt.stage == "419-hatch" | trt.stage == "433-hatch") %>% 
    mutate(trt.stage = factor(trt.stage, levels = c("260-hatch", "419-hatch", "337-hatch", "433-hatch"))) %>%
    filter(ID > 1170)
    #filter(expt.group %in% c("C", "D", "E", "F", "G", "H"))
    #filter(expt.group == "C" | expt.group == "D" | expt.group == "E" | expt.group == "F" | expt.group == "G" | expt.group == "H" | expt.group == "I")
    
  return(filtered_data)
}


### for larval instars (4th - pup)
  # idk if this gets used much tho tbh
filter.ins.topup <- function(data) {
  filtered_data <- data %>%
    filter(instar == "4th" | instar == "5th" | instar == "6th" | instar == "7th" | instar == "wander" | instar == "pupa")
  
  return(filtered_data)
}



# doing math ######

# calc dev summ stats

calc.devsumm <- function(data) {
  summary <- data %>%
    summarise(avg.mass = mean(na.omit(mass)),
              se.mass = sd(na.omit(mass))/sqrt(length(na.omit(mass))),
              avg.tt = mean(na.omit(tt)),
              se.tt = sd(na.omit(tt))/sqrt(length(na.omit(tt))),
              logmass = log(na.omit(mass)),
              avg.logmass = mean(na.omit(logmass)),
              se.logmass = sd(na.omit(logmass))/sqrt(length(na.omit(logmass))),
              n=n())
  return(summary)
}

calc.devsumm.trtstg <- function(data) {
  summary <- data %>%
    group_by(pop, trt.stage, instar) %>%
    summarise(avg.mass = mean(na.omit(mass)),
              se.mass = sd(na.omit(mass))/sqrt(length(na.omit(mass))),
              avg.tt = mean(na.omit(tt)),
              se.tt = sd(na.omit(tt))/sqrt(length(na.omit(tt))),
              logmass = log(mass),
              avg.logmass = mean(na.omit(logmass)),
              se.logmass = sd(na.omit(logmass))/sqrt(length(na.omit(logmass))),
              n=n())
  
  return(summary)
}

calc.devsumm.trtstgsex <- function(data) {
  summary <- data %>%
    group_by(pop, sex, trt.stage, instar) %>%
    summarise(avg.mass = mean(na.omit(mass)),
              se.mass = sd(na.omit(mass))/sqrt(length(na.omit(mass))),
              avg.tt = mean(na.omit(tt)),
              se.tt = sd(na.omit(tt))/sqrt(length(na.omit(tt))),
              logmass = log(mass),
              avg.logmass = mean(na.omit(logmass)),
              se.logmass = sd(na.omit(logmass))/sqrt(length(na.omit(logmass))),
              n=n())
  
  return(summary)
}


# adding geoms ######

### error bars
y_err_logmass <- function(err = 0.9) {
  list(geom_errorbar(aes(ymin = avg.logmass - se.logmass, ymax = avg.logmass + se.logmass), width = err))
}

y_err_mass <- function(err = 0.9) {
  list(geom_errorbar(aes(ymin = avg.mass - se.mass, ymax = avg.mass + se.mass), width = err))
}

y_err_tt <- function(err = 0.9){
  list(geom_errorbar(aes(ymin = avg.tt - se.tt, ymax = avg.tt + se.tt), width = err))
}

x_err_tt <- function(err = 0.9){
  list(geom_errorbarh(aes(xmin = avg.tt - se.tt, xmax = avg.tt + se.tt), height = err))
}

# # custom error bars: see 
# # https://stackoverflow.com/questions/18327466/ggplot2-error-bars-using-a-custom-function
# # default width/height is 0.9. see
# # https://stackoverflow.com/questions/28370249/correct-way-to-specifiy-optional-arguments-in-r-functions
#   # 240118: not working LMAO getting the data.frame fed in isnt working
# y_err <- function(data, y_stat, y_err, y_wid){ #ind and err are indices for the error columns
#   if(missing(y_wid)) {
#     y_wid <- 0.9
#   } else {
#     y_wid <- y_wid
#   }
#   
#   #rlang::eval_tidy(data <- data)
#   
#   #yerr_names <- c(y_stat, y_err)
#   
#   # yerr_names <- rlang::eval_tidy(data <- data) %>%
#   #   names(data)[c({{ystat_ind}}, {{yerr_ind}})]
#   
#   # y_errbars <- aes_string(ymin = paste(yerr_names, collapse = "-"),
#   #                        ymax = paste(yerr_names, collapse = "+"))
#   # 
#   # list(geom_errorbar(mapping = y_errbars), width = y_wid)
#   
#   list(geom_errorbar(data, aes(ymin = y_stat - y_err, ymax = y_stat + y_err), width = y_err))
# }

### recoloring theme + legend
  # i think these r dead tbh
temp_aes <- function(x)(
  list(theme_bw(), scale_color_manual(values=temp_colors, labels=temp_labels))
)

acc_aes <- function(x){
  list(theme_bw(), scale_color_manual(values=acc_colors, labels=acc_labels))  
}

NT_aes <- function(x){
  list(theme_bw(), scale_color_manual(values=NT_colors, labels=NT_labels))
}


# defining aesthetics ######

#all_trts <- c("260-hatch", "267-hatch", "330-hatch", "337-hatch", "337-3rd", "337-4th", "40-19", "40-26")


# temp: effect of mean/fluct temp (treatment)
temp_trts = c("260-hatch", "267-hatch", "330-hatch", "337-hatch")
temp_labels = c("260-hatch"="26°C", "267-hatch"="26±7°C", "330-hatch"="33°C", "337-hatch"="33±7°C")
temp_colors = c("260-hatch"="#00C2D1","267-hatch"="#1929B3", "330-hatch"="#F9C639", "337-hatch"="#710A36")



# accum: effect of temp x instar (trt.stage)
acc_trts = c("260-hatch", "337-hatch", "337-3rd", "337-4th")
acc_labels = c("260-hatch"="26°C @ hatch","337-hatch"="33±7°C @ hatch", "337-3rd"="33±7°C @ 3rd", "337-4th"="33±7°C @ 4th")
acc_colors = c("260-hatch"="#00C2D1", "337-hatch"="#710A36", "337-3rd"="#C23C1E", "337-4th"="#F3922B")
#CD133F


# NTs: same DTs, different NTs 
NT_trts = c("260-hatch", "419-hatch", "337-hatch", "433-hatch")
NT_labels = c("260-hatch"="26/26 (26±0°C)", "419-hatch"="40/19 (29.5±10.5°C)", "337-hatch"="40/26 (33±7°C)", "433-hatch"="40/33 (36.5±3.5°C)")
NT_colors = c("260-hatch"="#F4B942", "419-hatch"="#4059AD", "337-hatch"="#6B9AC4", "433-hatch"="#97D8C4")
# although i think 267 is the better comparison, i have more 260s

# for survival but lowkey need to edit it below
# A_hex = c("#00C2D1","#1929B3", "#F9C639", "#710A36")
# B_hex = c("#00C2D1", "#710A36", "#C23C1E", "#F3922B")
# C_hex = c("#F4B942", "#4059AD", "#6B9AC4", "#97D8C4")


# test color palette
RYB <- c("#324DA0", "#acd2bb", "#f1c363", "#a51122")



# cleanup -----------------------------------------------------------------

# for removing some expt-specific associated aesthetics, functions, etc
# to use, just run rm(list=HELPER)

temps_helpers <- c("temp_colors", "temp_labels", "temp_trts", "filter.temps2", "temp_aes", "temps_helpers")
acc_helpers <- c("acc_colors", "acc_labels", "acc_trts", "filter.acc2", "acc_aes", "acc_helpers")
NT_helpers <- c("NT_colors", "NT_labels", "NT_trts", "filter.NTs2", "NT_aes", "NT_helpers")
other_helpers <- list(temps_helpers, acc_helpers, NT_helpers, "other_helpers")
