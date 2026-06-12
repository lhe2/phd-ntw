# ntw 2025 utils
# 2025-11-28


### ABOUT ###

# general utility functions that are useful at across multiple scripts
# (mostly post-wrangle)

############


### FILTERS ###
# exclude lab bugs reared on TB
FilterOutLabTB <- function(data){
  data %>%
    filter(!(pop == "lab" & diet == "TB"))
}

# keep only mated lab and col moths
FilterForLabEggs <- function(data){
  data %>%
    filter(mate.pop != "field",
           mate.type != "virgin") 
}

# keeps trts 260, 419, 426, 433 only
# (drops other 2023 trts)
FilterForNTWTrts <- function(data){
  if(rlang::has_name(data, "is.ntw")){
    filter(data, is.ntw > 0)
  } else {
    filter(data, trt %in% c(260, 419, 426, 433))
  }
}

## combined filters
FilterForNTWBugs <- function(data){
  data %>% 
    FilterOutLabTB() %>% FilterForNTWTrts()
}

FilterForNTWMoths <- function(data){
  data %>% 
    FilterForLabEggs() %>% FilterForNTWTrts()
}
