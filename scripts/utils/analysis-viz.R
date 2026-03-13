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
# labels -- tho i think this can be all scale things. p_scales? scaleaes? aes?
p_scales <- list(
  labs_trt = c("260" = "26-26", "419" = "40-19", "426" = "40-26", "433" = "40-33"),
  labs_minT = c("260" = "26", "419" = "19", "426" = "26", "433" = "33"),
  
  cols_trt = c("260" = "#00A3B6", "419" = "#4B1D91", "426" = "#A71B4B", "433" = "#F9C25C")
  # sth for "as_labeller" once i settle on sth
  )
