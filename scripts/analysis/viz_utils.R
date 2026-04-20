# ntw 2025 viz utils
# 2025-11-28

### SCALE VALUES ###
p_scales <- list(
  # labels
  labs_trt = c("260" = "26-26", "419" = "40-19", "426" = "40-26", "433" = "40-33"),
  labs_minT = c("260" = "26", "419" = "19", "426" = "26", "433" = "33"),
  labs_trttype = c("ctrl" = "control (26-26°C)", "expt" = "nighttime warming\n(40-X°C)"),
  labs_trtsex = c("f" = "female only", "m" = "male only", "both" = "male + female"),
  
  # values
  cols_trt = c("260" = "#00A3B6", "419" = "#4B1D91", "426" = "#A71B4B", "433" = "#F9C25C"),
  cols_trttype = c("ctrl" = "#6BAED6", "expt" = "#FEB24C"),
  cols_trtsex = c("both" = "slateblue2", "f" = "maroon2", "m" = "deepskyblue",
                  "none" = "slateblue2", "virgin" = "maroon2"),
  
  shp_pop = c(`lab` = 19, `field` = 1),
  lty_pop = c(`lab` = "solid", `field` = "dashed"),
  shp_trtsex = c("both" = 16, "f" = 2, "m" = 0),
  
  # use with facet_*(labeller)
  facs_trtpop = c(`ctrl` = "control (26-26°C)",
                  `lab` = "lab (40-X°C)",
                  `field` = "field (40-X°C)"),
  facs_trttype = c(`ctrl` = "control (26-26°C)",
                   `expt` = "nighttime warming (40-X°C)"),
  facs_sex = c(`f` = "female", `m` = "male"),
  
  # use with do.call(scale_*_*, args)
  lty_ispup = list(labels = c(`1` = "survived", `0` = "died"),
                   values = c(`1` = "solid", `0` = "dashed"),
                   limits = c("1", "0"))
  )

### PLOTTING ###
## adding error bars 
# use with geom_errorbar/errorbarh to set width/height
# of the error bar to some % of the other axis
# (default value or custom)
# see also https://gist.github.com/tomhopper/9076152 for suggested defaults

# need to specify df$axis if not using the df passed into the original ggplot call
CalcErrWd <- function(x, pct = 0.12){
  max(x) * pct 
}

CalcErrHt <- function(y, pct = 0.07){
  max(y) * pct
}


