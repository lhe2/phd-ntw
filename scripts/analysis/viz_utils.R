# ntw 2025 viz utils
# 2025-11-28

### SCALE VALUES ###
p_scales <- list(
  # axes labels
  xlab_minT = "minimum temperature (°C)",  # TODO or "minimum larval temp"
  
  # labels
  labs_trt = c("260" = "26-26", "419" = "40-19", "426" = "40-26", "433" = "40-33"),
  labs_minT = c("260" = "26", "419" = "19", "426" = "26", "433" = "33"),
  labs_trttype = c("ctrl" = "control (max: 26°C)", "expt" = "nighttime warming\n(max: 40°C)"),
  labs_trtsex = c("f" = "female only", "m" = "male only", "both" = "male + female"),
  labs_trtmate = c("both" = "both", "f" = "female", "m" = "male", "neither" = "neither (control)"),
  
  # values
  cols_trt = c("260" = "#00A3B6", "419" = "#4B1D91", "426" = "#A71B4B", "433" = "#F9C25C"),
  cols_trttype = c(#"ctrl" = "#6BAED6", 
                   "ctrl" = "steelblue1",
                   "expt" = "#FEB24C"),
  cols_trtsex = c("both" = "slateblue2", "f" = "maroon2", "m" = "deepskyblue", 
                  "neither" = "slateblue4",
                  "none" = "slateblue2", "virgin" = "maroon2"),
  
  shp_pop = c(`lab` = 16, `field` = 17),
  lty_pop = c(`lab` = "solid", `field` = "dashed"),
  shp_trtsex = c(#"both" = 16, "f" = 2, "m" = 0, "neither" = 4, # hard to see...
                 "both" = 16, "f" = 17, "m" = 15, "neither" = 1),
  lty_trtsex = c("both" = "solid", "f" = "dashed", "m" = "dotdash", "neither" = "blank"),
  
  # use with facet_*(labeller)
  facs_trtpop = c(`ctrl` = "control (max: 26°C)",
                  `lab` = "lab (max: 40°C)",
                  `field` = "field (max: 40°C)"),
  facs_trttype = c(`ctrl` = "control (max: 26°C)",
                   `expt` = "nighttime warming (max: 40°C)"),
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


