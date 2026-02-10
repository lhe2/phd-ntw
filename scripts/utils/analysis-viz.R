# ntw 2025 viz utils
# 2025-11-28

### ERROR BARS ###
# use with geom_errorbar/errorbarh to set width/height
# of the error bar to some % of the other axis
# (default value or custom)
# see also https://gist.github.com/tomhopper/9076152 for suggested defaults

CalcErrWd <- function(x, pct = 0.12){
  max(x) * pct 
}

CalcErrHt <- function(y, pct = 0.07){
  max(y) * pct
}

### MISC ###
p_labs <- list(
  temps = c("260" = "26-26", "419" = "40-19", "426" = "40-26", "433" = "40-33")
  # sth for "as_labeller" once i settle on sth
  )