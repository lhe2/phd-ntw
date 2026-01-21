# ntw 2025 utils
# 2025-11-28


### ABOUT ###

# general utility functions that are useful at across multiple scripts
# (mostly post-wrangle)

############


### FILTERS ###
FilterOutLabTB <- function(data){
  data %>%
    filter(!(pop == "lab" & diet == "TB"))
}

FilterForLabEggs <- function(data){
  data %>%
    filter(mate.pop != "field",
           mate.type != "virgin f") 
}

# keeps trts 260, 419, 426, 433 only
FilterForNTW <- function(data){
  data %>%
    filter(is.ntw > 0)
}







