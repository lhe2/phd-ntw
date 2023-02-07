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


# setup -------------------------------------------------------------------

library(dplyr)
library(ggplot2)
#library(readr)

data <- read.csv("~/Documents/projects/data/ntw_data/development.csv", header = T)
  # remove empty lines...  

## add julian columns
# change current columns to date format

data <- data[,-18] # remove "X" column
data <- rename(data, date.hatch = hatch.date) # for consistency

data$date.hatch <- as.Date(data$date.hatch, format = "%m/%d/%y")
data$date.2nd <- as.Date(data$date.2nd, format = "%m/%d/%y")
data$date.3rd <- as.Date(data$date.3rd, format = "%m/%d/%y")
data$date.4th <- as.Date(data$date.4th, format = "%m/%d/%y")
data$date.5th <- as.Date(data$date.5th, format = "%m/%d/%y")
data$date.wander <- as.Date(data$date.wander, format = "%m/%d/%y")  

# make new julian columns
data$jdate.hatch <- format(data$date.hatch, "%j")

# light analysis ----------------------------------------------------------

# for 230207: look at average instar masses + time to instars for all groups (no filtering)


  