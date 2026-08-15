# global wrangle util fns
# 2026-03-11

# aka the summary stats fns

### SUMMARY STATS ###
# standard error
se <- function(x){ 
  sd(na.omit(x))/sqrt(length(na.omit(x)))
}

# standard error of a proportion
# where p = x/n
seprop <- function(p, n){
  sqrt((p*(1-p))/n)
}


## PLOTTING/PIVOTING ##

# see the tidy/dev.Rmd for DevToLong() (bc stats uses long data)

# pivoting for more condensed plotting of SS
# generic: mostly for dev data but works for anything with the "avg.*", "se.*" name format
SSToLong <- function(wide_ss){
  wide_ss %>%
    pivot_longer(cols = starts_with(c("avg", "se")),
                 names_to = c(".value", "response"),
                 names_pattern = "^(avg|se)\\.(.*)")
}
