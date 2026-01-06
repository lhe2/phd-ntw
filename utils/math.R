# math utils.R
# created: 2026-01-01

# frequently used calculation fns

# std error
se <- function(x){ 
  sd(na.omit(x))/sqrt(length(na.omit(x)))
}

# std error of a proportion
# where p = x/n
se.prop <- function(p, n){
  sqrt((p*(1-p))/n)
}