# analysis-tdt_utils.R
# 2025-08-09

# helper functions for tdt data analyses

# psa i am rlly indecisive abt creating these things,
# (and kept running into a lot of errors) -- 
# so any "new" versions of fns should be placed on top of the previous one

# todo is to go clean this up LOL... (maybe need to consolidate some of the more outdated stuff together,
# e.g. cohort A things...)


# calc status and dh at certain expt timepoints ---------------------------

# _days = time in days
# _hours = time in hours. generally use this one

calc.timepointbins_days <- function(widedata){
  widedata %>% 
    mutate(dh.enter48 = dh.enter + 2,
           dh.return48 = case_when(trt > 100 ~ dh.return + 2,
                                   trt < 100 ~ dh.enter48 + 2),
           
           status.enter = case_when(dh.exit > dh.enter ~ 1,
                                    TRUE ~ 0),
           status.enter48 = case_when(dh.exit > dh.enter48 ~ 1,
                                      TRUE ~ 0),
           status.return = case_when(trt > 100 & dh.exit > dh.return ~ 1,
                                     trt < 100 ~ NA_real_,
                                     TRUE ~ 0),
           status.return48 = case_when(dh.exit > dh.return48 ~ 1,
                                       TRUE ~ 0)) %>%
    return()
}
## for cohort B+ data (symmetrical around 24h) ---------------------------

# (symmetry around 24h bc i culled B after 24h after the last 40 died)

calc.timepointbins_hoursB2 <- function(widedata){
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

calc.timepointbins_hoursB <- function(widedata){
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

## for cohort A data (symmetrical around 48h) ---------------------------
calc.timepointbins_hoursA <- function(widedata){
  widedata %>% 
    mutate(dh.enter48 = dh.enter + 48,
           dh.return48 = case_when(trt > 100 ~ dh.return + 48,
                                   trt < 100 ~ dh.enter48 + 48),
           dh.return72 = case_when(trt > 100 ~ dh.return + 72,
                                   trt < 100 ~ dh.enter48 + 72),
           
           status.enter = case_when(dh.exit > dh.enter ~ 1,
                                    TRUE ~ 0),
           status.enter48 = case_when(dh.exit > dh.enter48 ~ 1,
                                      TRUE ~ 0),
           status.return = case_when(trt > 100 & dh.exit > dh.return ~ 1,
                                     trt < 100 ~ NA_real_,
                                     TRUE ~ 0),
           status.return48 = case_when(dh.exit > dh.return48 ~ 1,
                                       TRUE ~ 0),
           status.return72 = case_when(dh.exit > dh.return72 ~ 1,
                                       TRUE ~ 0)) %>%
    return()
}



# calc counts and surv props ---------------------------

calc.surv_ssB2a <- function(widedata){
  widedata %>%
    #dfs$r1$allB %>% # tester data
    calc.timepointbins_hoursB2() %>% #View()
    group_by(trt, trt.duration, trt.recover) %>%
    summarise(n.surv.enter = sum(status.enter == 1),
              n.surv.enter24 = sum(status.enter24 == 1),
              n.surv.hs0 = sum(status.hs0 == 1),
              n.surv.hs24 = sum(status.hs24 == 1),
              n.surv.hs48 = sum(status.hs48 == 1),
              n.surv.hs72 = sum(status.hs72 == 1),
              
              # count death if dead at current timept but alive at previous major timept
              n.died.enter24 = sum(status.enter24 == 0),
              n.died.hs0 = sum(status.hs0 == 0 & status.enter24 == 1),
              n.died.hs24 = case_when(trt.duration == 0 ~ sum(status.hs24 == 0 & status.enter24 == 1),
                                      TRUE ~ sum(status.hs24 == 0 & status.hs0 == 1)),
              n.died.hs48 = case_when(trt.duration == 0 ~ sum(status.hs48 == 0 & status.enter24 == 1),
                                      TRUE ~ sum(status.hs48 == 0 & status.hs24 == 1)),
              n.died.hs72 = case_when(trt.duration == 0 ~ sum(status.hs72 == 0 & status.enter24 == 1),
                                      TRUE ~ sum(status.hs72 == 0 & status.hs48 == 1)),
              
              # prop surv = alive at current timept/total entering initial timept
              prop.surv.enter24 = n.surv.enter24/n.surv.enter,
              prop.surv.hs0 = n.surv.hs0/n.surv.enter24,
              prop.surv.hs24 = case_when(trt.duration == 0 ~ n.surv.hs24/n.surv.enter24,
                                         TRUE ~ n.surv.hs24/n.surv.hs0),
              prop.surv.hs48 = n.surv.hs48/n.surv.hs24,
              prop.surv.hs72 = n.surv.hs72/n.surv.hs48,
              
              prop.died.enter24 = 1 - prop.surv.enter24,
              prop.died.hs0 = 1 - prop.surv.hs0,
              prop.died.hs24 = 1 - prop.surv.hs24,
              prop.died.hs48 = 1 - prop.surv.hs48,
              prop.died.hs72 = 1 - prop.surv.hs72) %>%
    pivot_longer(cols = starts_with(c("n.", "prop")),
                 names_to = c(".value", "status", "timept"), names_sep = "\\.") %>%
    unique()
}

calc.surv_ssB2 <- function(widedata){
  widedata %>%
    #dfs$r1$allB %>% # tester data
    calc.timepointbins_hoursB2() %>% #View()
    group_by(trt, trt.duration, trt.recover) %>%
    summarise(n.surv.enter = sum(status.enter == 1),
              n.surv.enter24 = sum(status.enter24 == 1),
              n.surv.hs0 = sum(status.hs0 == 1),
              n.surv.hs24 = sum(status.hs24 == 1),
              n.surv.hs48 = sum(status.hs48 == 1),
              n.surv.hs72 = sum(status.hs72 == 1),
              
              # count death if dead at current timept but alive at previous major timept
              n.died.enter24 = sum(status.enter24 == 0),
              n.died.hs0 = sum(status.hs0 == 0 & status.enter24 == 1),
              n.died.hs24 = case_when(trt.duration == 0 ~ sum(status.hs24 == 0 & status.enter24 == 1),
                                      TRUE ~ sum(status.hs24 == 0 & status.hs0 == 1)),
              n.died.hs48 = case_when(trt.duration == 0 ~ sum(status.hs48 == 0 & status.enter24 == 1),
                                      TRUE ~ sum(status.hs48 == 0 & status.hs24 == 1)),
              n.died.hs72 = case_when(trt.duration == 0 ~ sum(status.hs72 == 0 & status.enter24 == 1),
                                      TRUE ~ sum(status.hs72 == 0 & status.hs48 == 1)),
              
              # prop surv = alive at current timept/total entering initial timept
              prop.surv.enter24 = n.surv.enter24/n.surv.enter,
              prop.surv.hs0 = n.surv.hs0/n.surv.enter24,
              prop.surv.hs24 = case_when(trt.duration == 0 ~ n.surv.hs24/n.surv.enter24,
                                         TRUE ~ n.surv.hs24/n.surv.hs0),
              prop.surv.hs48 = case_when(trt.duration == 0 ~ n.surv.hs48/n.surv.enter24,
                                         TRUE ~ n.surv.hs48/n.surv.hs0),
              prop.surv.hs72 = case_when(trt.duration == 0 ~ n.surv.hs72/n.surv.enter24,
                                         TRUE ~ n.surv.hs72/n.surv.hs0),
              
              prop.died.enter24 = 1 - prop.surv.enter24,
              prop.died.hs0 = 1 - prop.surv.hs0,
              prop.died.hs24 = 1 - prop.surv.hs24,
              prop.died.hs48 = 1 - prop.surv.hs48,
              prop.died.hs72 = 1 - prop.surv.hs72) %>%
    pivot_longer(cols = starts_with(c("n.", "prop")),
                 names_to = c(".value", "status", "timept"), names_sep = "\\.") %>%
    unique()
}

calc.surv_ssB <- function(widedata){
  widedata %>%
    #dfs$r1$allB %>% # tester data
    calc.timepointbins_hoursB() %>% #View()
    group_by(trt, trt.duration, trt.recover) %>%
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

