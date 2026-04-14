# ntw 2025 stats utils
# 2025-11-28

# checks if the object being passed in is a list or not
# (i.e. list of fitted model objs or single obj -- converts to list if not)


# OUTPUT ------------------------------------------------------------------

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


# (WIP) write csv of anova results
## USAGE (WriteModCsv)
  # writes csvs of outputs for single model objects or for bound lists of multiple models.
## ARGS
  # mod: pass in actual model object/just the filename (var names break if using dot)
  # filename: desired file name as a string
WriteModelResultsCsv <- function(mod, filename){
  path <- paste0("_ntw/figs/out/", filename, ".csv")
  
  if(is.data.frame(mod) == FALSE){
    res <- anova(mod, test = "Chisq") %>% as.data.frame()
    write.csv(res, here::here(path))
  } else if (is.data.frame(mod) == TRUE) { # already bound list
    write.csv(mod, here::here(path), row.names = FALSE)
  } else { # if list of dfs to be rbindlisted
    res <- rbindlist(mod)
    write.csv(res, here::here(path), row.names = FALSE)
  }
}

# DIAGNOSE ----------------------------------------------------------------

# generate fitted vs resid; QQ plot
## (need to load ggfortify if not working)
DiagnoseModel <- function(mod){
  if(inherits(mod, "list") == FALSE){
    mod <- list(mod)
  } 
  
  mod %>% lapply(., \(x){
    list(
      # gridExtra::grid.arrange(grobs = autoplot(x, which = 1:2, na.action="fail")@plots,
      #                            top = x$formula)
      autoplot(x, which = 1:2, na.action="fail") + labs(caption = x$formula)
      )
  })
  
  ## trying to shove in functionality for the zeroinfl models
  # mods %>% lapply(., \(x){
  #   ifelse(!(class(x) %in% c("lm", "glm")), # needs to be vectorised..
  #          {qqnorm(resid(x));qqline(resid(x))}, 
  #          list(autoplot(x, which = 1:2, na.action="fail")) 
  #           # not sure how to make this only give back 1 plot instead of dups...
  #   )
  # })
}

# for non glms/lms
DiagnoseModel2 <- function(mod){
  if(inherits(mod, "list") == FALSE){
    mod <- list(mod)
  }
  
  mod %>% lapply(., \(x){
    fitted <- fitted(x)
    res <- resid(x)
    
    ## resid vs fitted
    plot(fitted, res)
    lines(lowess(fitted, res), col = "red")
    
    ## qq
    qqnorm(res); qqline(res)
  })
}