# ntw 2025 utils
# 2025-11-28


# readme ------------------------------------------------------------------

# utility and user-defined functions for ntw wrangling/analyses/etc 
# cuz i always lose them in the sauce


# WRANGLE -----------------------------------------------------------------
se <- function(x){ 
  sd(na.omit(x))/sqrt(length(na.omit(x)))
}

# where p = x/n
se.prop <- function(p, n){
  sqrt((p*(1-p))/n)
}


# ANALYSIS ----------------------------------------------------------------
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


### STATS ###
GetModelResults <- function(x){
  list(
    model_summary = summary(x),
    #anova_results = anova(x),
    anova_wchisq = anova(x, test = "Chisq")
  )
}


### VIZ (GGPLOT) ###
# sets error bar width/height to some % of the other axis
## struggling w ggplot error bar width issues again... 
## see also https://gist.github.com/tomhopper/9076152 for suggested defaults

CalcErrWd <- function(x, pct = 0.12){
  max(x) * pct 
}

CalcErrHt <- function(y, pct = 0.07){
  max(y) * pct
}







