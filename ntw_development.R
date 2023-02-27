## // TODO

# processing TODO
  # convert date.# to julian
  # new days to... (from hatching) columns for all instars

# stats
  # avg weight at instar
  # avg length to instar from hatching

# subsetting (in order of priority)
# (mostly bc weight at instar is affected by timing of the measurement)
  # 1. aggregate (no addtl considerations)
  # 2. hcs stage
    # 2a. keep p/h only
    # 2b. keep p/h/hm only
  # etc. initial temperature considerations
    ## (not too sure about this one bc N is pretty small?)
    # 267: 0, 1 (1 is pretty similar)
    # 330: 0, 1 (ptnlly worth filtering)
    # 337: 1, 2 (ptnlly worth filtering)

# survival stats


# setup -------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(tidyr)
#library(readr)

data_all <- read.csv("~/Documents/projects/data/ntw_data/development.csv", header = T)
  # remove empty lines...  

# initial cleaning: making some subsets, removing some samples
data_notes <- data_all[c(1:3,31)] # save data notes elsewhere

data_pmds <- filter(data_all, date.pmd != "") # save pmd info elsewhere
data_pmds <- data_pmds[-2,] # remove UID 1007 bc never existed
data_pmds <- filter(data_pmds, notes != "squished") # remove ones that got squished as 1sts

# final cleanup of main analysis df
data <- data_all %>% filter(date.pmd == "") %>% select(-c("notes", "date.15"))

  # // TODO: calculate # pmds

# pivot longer
#longtest <- pivot_longer(data, cols = starts_with(c("date", "mass", "h")),
#                         names_to = c("date", "mass", "molt_status"),
#                         names_sep = ("."),
#                         names_prefix = c("date.", "mass.", "h."))

data <- rename(data, date.hatch = hatch.date) # for consistency (// TODO: fix this in main data sheet)

# // TODO: autoconvert all dates to julian (so i dont need to do this when updating the csv every time)

longtest <- pivot_longer(data, cols = starts_with(c("date", "mass", "h")),
                        names_to = c(".value", "instar"),
                        #names_sep = ".",
                        values_drop_na = TRUE,
                        names_pattern = ("([a-z]*)\\.(\\d*[a-z]*)"))

longtest <- longtest %>% rename(molt.status = h) %>%
  mutate(date = as.Date(date, format = "%m/%d/%y"),
         jdate = as.numeric(format(date, "%j")))

## add julian columns
# change current columns to date format
#data <- data[,-18] # remove "X" column

data$date.hatch <- as.Date(data$date.hatch, format = "%m/%d/%y")
data$date.2nd <- as.Date(data$date.2nd, format = "%m/%d/%y")
data$date.3rd <- as.Date(data$date.3rd, format = "%m/%d/%y")
data$date.4th <- as.Date(data$date.4th, format = "%m/%d/%y")
data$date.5th <- as.Date(data$date.5th, format = "%m/%d/%y")
data$date.wander <- as.Date(data$date.wander, format = "%m/%d/%y")  
data$date.pupate <- as.Date(data$date.pupate, format = "%m/%d/%y") 

# make new julian columns
data$jdate.hatch <- as.numeric(format(data$date.hatch, "%j"))
data$jdate.2nd <- as.numeric(format(data$date.2nd, "%j"))
data$jdate.3rd <- as.numeric(format(data$date.3rd, "%j"))
data$jdate.4th <- as.numeric(format(data$date.4th, "%j"))
data$jdate.5th <- as.numeric(format(data$date.5th, "%j"))
data$jdate.wander <- as.numeric(format(data$date.wander, "%j")) # breaks (ie doesnt format) if empty?
data$jdate.pupate <- as.numeric(format(data$date.pupate, "%j"))

# light analysis ----------------------------------------------------------

# for 230207: look at average instar masses + time to instars for all groups (no filtering)

### average masses
mass <- data %>% group_by(treatment) %>%
  summarise(n = n(),
            avg.mass.3rd = mean(mass.3rd, na.rm=T), sd.mass.3rd = sd(mass.3rd, na.rm=T),
            avg.mass.4th = mean(mass.4th, na.rm=T), sd.mass.4rd = sd(mass.4th, na.rm=T),
            avg.mass.5th = mean(mass.5th, na.rm=T), sd.mass.5th = sd(mass.5th, na.rm=T),
            avg.mass.wander = mean(mass.wander, na.rm=T), sd.mass.wander = sd(mass.wander, na.rm=T))

### average time between instars
# make new "time to..." for each instar
time <- mutate(data, time.to.2nd = jdate.2nd - jdate.hatch,
                    time.to.3rd = jdate.3rd - jdate.hatch,
                    time.to.4th = jdate.4th - jdate.hatch,
                    time.to.5th = jdate.5th - jdate.hatch,
                    time.to.wander = jdate.wander - jdate.hatch,
                    time.to.pupate = jdate.pupate - jdate.hatch)%>%
       group_by(treatment) %>%
       summarise(n = n(),
                 avg.time.to.2nd = mean(time.to.2nd, na.rm=T), sd.time.to.2nd = sd(time.to.2nd, na.rm=T),
                 avg.time.to.3rd = mean(time.to.3rd, na.rm=T), sd.time.to.3rd = sd(time.to.3rd, na.rm=T),
                 avg.time.to.4th = mean(time.to.4th, na.rm=T), sd.time.to.4th = sd(time.to.4th, na.rm=T),
                 avg.time.to.5th = mean(time.to.5th, na.rm=T), sd.time.to.5th = sd(time.to.5th, na.rm=T),
                 avg.time.to.wander = mean(time.to.wander, na.rm=T), sd.time.to.wander = sd(time.to.wander, na.rm=T),
                 avg.time.to.pupate = mean(time.to.pupate, na.rm=T), sd.time.to.pupate = sd(time.to.pupate, na.rm=T))

allstats <- merge(mass, time, by=c("treatment", "n"))

### plots

#mass.plot <- ggpplot(stats, aes(x=as.factor(treatment), y=))
  # hit a snag when i realised the y won't work the way i want it to here...

  