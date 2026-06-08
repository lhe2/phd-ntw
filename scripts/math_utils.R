# global wrangle util fns
# 2026-03-11

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