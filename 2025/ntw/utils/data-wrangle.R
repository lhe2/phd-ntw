# ntw 2025 wrangle utils
# 2025-11-28

### SUMMARY STATS ###
se <- function(x){ 
  sd(na.omit(x))/sqrt(length(na.omit(x)))
}

# where p = x/n
se.prop <- function(p, n){
  sqrt((p*(1-p))/n)
}