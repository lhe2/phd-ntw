# ntw 2025 stats utils
# 2025-11-28


# simultaneously return model summary and anova results
GetModelResults <- function(x){
  list(
    model_summary = summary(x),
    #anova_results = anova(x),
    anova_wchisq = anova(x, test = "Chisq")
  )
}