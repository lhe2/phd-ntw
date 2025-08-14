# analysis-tdt_utils.R
# 2025-08-09

# info ---------------------------

# helper/utility functions for tdt data analyses

# usage
## internal utils = for use within this script
## external utils = for use in analysis/viz scripts
## newer versions of fns should be written above of the previous one as they are developed

# TODO
  # rename things to what they are instead of just "B" and "B2" lol



# internal utils ---------------------------

## survival status coding --------------------------------------------------

# codes status and dh at specified expt timepoints 
# for use with cohort B+ data (symmetrical around 24h)
# (symmetry around 24h bc i culled B after 24h after the last 40 died)

# timept names (in hrs) based off hs
int.code.survbins_hoursB2 <- function(widedata){
  widedata %>% 
    mutate(#dh.enter = dh.enter,
      dh.enter24 = case_when(trt < 100 ~ dh.enter + 24,
                             trt > 100 ~ dh.recover),
      #dh.return = dh.return,
      dh.hs24 = case_when(trt > 100 ~ dh.return + 24,
                          trt < 100 ~ dh.enter24 + 24),
      dh.hs48 = dh.hs24 + 24,
      dh.hs72 = dh.hs48 + 24,
      
      status.enter = case_when(dh.exit > dh.enter ~ 1,
                               TRUE ~ 0),
      status.enter24 = case_when(#(trt < 100 & dh.exit > dh.enter24) | (trt > 100 & dh.exit > dh.recover) ~ 1,
        dh.exit > dh.enter24 ~ 1,
        TRUE ~ 0),
      status.hs0 = case_when(trt < 100 ~ NA_real_, 
                             trt > 100 & dh.exit > dh.return ~ 1,
                             TRUE ~ 0),
      status.hs24 = case_when(dh.exit > dh.hs24 ~ 1,
                              TRUE ~ 0),
      status.hs48 = case_when(dh.exit > dh.hs48 ~ 1,
                              TRUE ~ 0),
      status.hs72 = case_when(dh.exit > dh.hs72 ~ 1,
                              TRUE ~ 0)) %>%
    return()
}

# timept names (in hrs) based off enter/returning from recovery
int.code.survbins_hoursB <- function(widedata){
  widedata %>% 
    mutate(dh.enter24 = case_when(trt > 100 ~ dh.recovery,
                                  trt < 100 ~ dh.enter + 24),
           dh.return24 = case_when(trt > 100 ~ dh.return + 24,
                                   trt < 100 ~ dh.enter24 + 24),
           dh.return48 = dh.return24 + 24,
           dh.return72 = dh.return48 + 24,
           
           status.enter = case_when(dh.exit > dh.enter ~ 1,
                                    TRUE ~ 0),
           status.enter24 = case_when(dh.exit > dh.enter24 ~ 1,
                                      TRUE ~ 0),
           status.return = case_when(trt > 100 & dh.exit > dh.return ~ 1,
                                     trt < 100 ~ NA_real_,
                                     TRUE ~ 0),
           status.return24 = case_when(dh.exit > dh.return24 ~ 1,
                                       TRUE ~ 0),
           status.return48 = case_when(dh.exit > dh.return48 ~ 1,
                                       TRUE ~ 0),
           status.return72 = case_when(dh.exit > dh.return72 ~ 1,
                                       TRUE ~ 0)) %>%
    return()
}


## summary stats counts ----------------------------------------------------

# generic survival stats counter/summariser + pivoter function
  # TODO sth is breaking in here tho based off my line plots... (bc things are going down)
int.ss.count_n_pivot <- function(widedata){
  summarise(n.surv.enter = sum(status.enter == 1),
            n.surv.enter24 = sum(status.enter24 == 1),
            n.surv.return = sum(status.return == 1),
            n.surv.return24 = sum(status.return24 == 1),
            n.surv.return48 = sum(status.return48 == 1),
            n.surv.return72 = sum(status.return72 == 1),
            
            # count death if dead at current timept but alive at previous major timept
            n.died.enter24 = sum(status.enter24 == 0),
            n.died.return = sum(status.return == 0 & status.enter24 == 1),
            n.died.return24 = case_when(trt.duration == 0 ~ sum(status.return24 == 0 & status.enter24 == 1),
                                        TRUE ~ sum(status.return24 == 0 & status.return == 1)),
            n.died.return48 = case_when(trt.duration == 0 ~ sum(status.return48 == 0 & status.enter24 == 1),
                                        TRUE ~ sum(status.return48 == 0 & status.return == 1)),
            n.died.return72 = case_when(trt.duration == 0 ~ sum(status.return72 == 0 & status.enter24 == 1),
                                        TRUE ~ sum(status.return72 == 0 & status.return == 1)),
            
            # prop surv = alive at current timept/total entering previous timept
            prop.surv.enter24 = n.surv.enter24/n.surv.enter,
            prop.surv.return = n.surv.return/n.surv.enter24,
            prop.surv.return24 = case_when(trt.duration == 0 ~ n.surv.return24/n.surv.enter24,
                                           TRUE ~ n.surv.return24/n.surv.return),
            prop.surv.return48 = case_when(trt.duration == 0 ~ n.surv.return48/n.surv.enter24,
                                           TRUE ~ n.surv.return48/n.surv.return),
            prop.surv.return72 = case_when(trt.duration == 0 ~ n.surv.return72/n.surv.enter24,
                                           TRUE ~ n.surv.return72/n.surv.return),
            
            prop.died.enter24 = 1 - prop.surv.enter24,
            prop.died.return = 1 - prop.surv.return,
            prop.died.return24 = 1 - prop.surv.return24,
            prop.died.return48 = 1 - prop.surv.return48,
            prop.died.return72 = 1 - prop.surv.return72) %>%
    
    pivot_longer(cols = starts_with(c("n.", "prop")),
                 names_to = c(".value", "status", "timept"), names_sep = "\\.") %>%
    unique()
}

## archive -----------------------------------------------------------------

## timepoints (hrs) for cohort A data only (symmetrical around 48h) 
  ## not actually used anywhere bc see the R1 viz code for development of this function
  ## TODO so maybe clean up the R1 viz code so this one can be used?
# code.survbins_hoursA <- function(widedata){
#   widedata %>% 
#     mutate(dh.enter48 = dh.enter + 48,
#            dh.return48 = case_when(trt > 100 ~ dh.return + 48,
#                                    trt < 100 ~ dh.enter48 + 48),
#            dh.return72 = case_when(trt > 100 ~ dh.return + 72,
#                                    trt < 100 ~ dh.enter48 + 72),
#            
#            status.enter = case_when(dh.exit > dh.enter ~ 1,
#                                     TRUE ~ 0),
#            status.enter48 = case_when(dh.exit > dh.enter48 ~ 1,
#                                       TRUE ~ 0),
#            status.return = case_when(trt > 100 & dh.exit > dh.return ~ 1,
#                                      trt < 100 ~ NA_real_,
#                                      TRUE ~ 0),
#            status.return48 = case_when(dh.exit > dh.return48 ~ 1,
#                                        TRUE ~ 0),
#            status.return72 = case_when(dh.exit > dh.return72 ~ 1,
#                                        TRUE ~ 0)) %>%
#     return()
# }

## calc timepoints in days
  ## not actually used anywhere...
# code.timepointbins_days <- function(widedata){
#   widedata %>% 
#     mutate(dh.enter48 = dh.enter + 2,
#            dh.return48 = case_when(trt > 100 ~ dh.return + 2,
#                                    trt < 100 ~ dh.enter48 + 2),
#            
#            status.enter = case_when(dh.exit > dh.enter ~ 1,
#                                     TRUE ~ 0),
#            status.enter48 = case_when(dh.exit > dh.enter48 ~ 1,
#                                       TRUE ~ 0),
#            status.return = case_when(trt > 100 & dh.exit > dh.return ~ 1,
#                                      trt < 100 ~ NA_real_,
#                                      TRUE ~ 0),
#            status.return48 = case_when(dh.exit > dh.return48 ~ 1,
#                                        TRUE ~ 0)) %>%
#     return()
# }



# external utils ---------------------------

## surv summary stat df generation ---------------------------

# generating summary stats by different groups
# adds grouping by cohort
calc.surv_ssB2b <- function(widedata){
  widedata %>%
    #dfs$r1$allB %>% # tester data
    int.code.survbins_hoursB2() %>% #View()
    group_by(cohort, trt, trt.duration, trt.recover) %>%
    int.ss.count_n_pivot()
}

calc.surv_ssB2a <- function(widedata){
  widedata %>%
    #dfs$r1$allB %>% # tester data
    int.code.survbins_hoursB2() %>% #View()
    group_by(trt, trt.duration, trt.recover) %>%
    int.ss.count_n_pivot()
}

calc.surv_ssB2 <- function(widedata){
  widedata %>%
    #dfs$r1$allB %>% # tester data
    int.code.survbins_hoursB2() %>% #View()
    group_by(trt, trt.duration, trt.recover) %>%
    int.ss.count_n_pivot()
}

calc.surv_ssB <- function(widedata){
  widedata %>%
    #dfs$r1$allB %>% # tester data
    int.code.survbins_hoursB() %>% #View()
    group_by(trt, trt.duration, trt.recover) %>%
    int.ss.count_n_pivot()
}
