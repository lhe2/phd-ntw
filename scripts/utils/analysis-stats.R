# ntw 2025 stats utils
# 2025-11-28

# checks if the object being passed in is a list or not
# (i.e. list of fitted model objs or single obj -- converts to list if not)

# simultaneously return model summary and anova results
GetModelResults <- function(x){
  if(inherits(x, "list") == FALSE){
    x <- list(x)
  }
  
  x %>% lapply(., \(x){
    list(model_summary = summary(x),
         anova_wchisq = anova(x, test = "Chisq"))
    })
}

# generate fitted vs resid; QQ plot
## (need to load ggfortify if not working)
DiagnoseModel <- function(x){
  if(inherits(x, "list") == FALSE){
    x <- list(x)
  } 
  
  x %>% lapply(., \(x){
    list(autoplot(x, which = 1:2, na.action="fail"))
  })
}

