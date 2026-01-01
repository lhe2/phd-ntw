# analysis-tdt_utils.R
# 2025-08-09

# info ---------------------------

# helper/utility functions for tdt data analyses
# (currently these are all just for the survival proportions viz)

# km utils =========================

# create kms
# takes pre-subsetted/filtered data and
# summarises the enter/recover/return date for km plots
calc.kmtimes <- function(filtereddf, ...){
  # ... = vars to group_by
  
  # TODO wanted to make this automatically update dfs$group here but maybe another time
  filtereddf %>%
    group_by(!!!rlang::ensyms(...)) %>%
    summarise(day.enter, day.recover, day.return) %>%
    drop_na() %>% unique()
# recalcs the start of km curves (T=0) given a day (i.e. day.recover or day.return)
# turns negative values NA so that viz.kmtimes() will be happy
calc.kmtimes0 <- function(df, day0){
  df %>%
  mutate_all(~ . - {{day0}}) %>%
    mutate(across(everything(), function(x) replace(x, which(x<0), NA))) %>% unique()
}

# add the enter/recover/return time vertical lines to km plots
# add these geoms last and include the %++% operator after the initial ggsurvplot() call
  # see ??add_ggsurvplot: this is bc ggplot2 and ggsurvplot dont play nice, 
  # and bc im calling "color" both as a variable in different ways...
viz.kmtimes <- function(kmgroup) {
  #kmgroup = basically, should be whatever dfs$group is from calc.kmtimes()
  list(geom_vline(data = kmgroup, aes(xintercept = day.enter), color = "red"),
       geom_vline(data = kmgroup, aes(xintercept = day.recover), color = "skyblue2"),
       geom_vline(data = kmgroup, aes(xintercept = day.return), color = "orange")
  )
}


# surv props util fns ==============================


# usage
## internal fns (.fn) = for use within the external helper fns
  ## code.survbins:
    # codes status and dh at specified expt timepoints 
    # for use with cohort B+ data (symmetrical around 24h)
    # (symmetry around 24h bc i culled B after 24h after the last 40 died)
  ## count_n_pivot:
    # generic survival stats counter/summariser + pivoter function

## external fns = compiled internal fns for use in analysis/viz scripts (starts with whatever)
  ## calc.surv:
    # combines the internal fns + group_by to generate summary stats for surv props viz
    # code.survbins > group_by (varies) > count_n_pivot


# organisation/versions
## fns have organised by overall "versions" (rather than by function) bc easier to scroll lol
## newer versions of fns should be written above of the previous one as they are developed
  # B = 2nd hs timepts named as "return"
  # B2 = 2nd hs timepts named as "hs"
  # C = standardised timept names; better handling of 40C bugs w the exptal ones

## ver C -------------------------

# better handling of all "hot" bugs
.code_survbinsC <- function(widedata){
  widedata %>%
    ## create additional timepts ##
    # dfs$data %>%  # testing
    mutate(
      # dh.enter24 exists for correct comparison of 40C ctrl to exptals...
      dh.enter24 = case_when(trt < 100 ~ dh.enter + 24,
                             TRUE ~ dh.recover),
      dh.hs0 = case_when(trt < 100 ~ dh.enter24,
                         TRUE ~ dh.return),
      dh.hs24 = dh.hs0 + 24,
      dh.hs48 = dh.hs24 + 24,
      dh.hs72 = dh.hs48 + 24) %>%
    
    ## code status ##
    mutate(
      across(c("dh.enter", "dh.enter24", "dh.hs0", "dh.hs24", "dh.hs48", "dh.hs72"), 
             ~ case_when(dh.exit > . ~ 1, TRUE ~ 0),
             .names = "{sub('dh', 'status', col)}"
             #.names = paste0("status.", str_split(., "dh."))
      )
    )
}

#.count_n_pivotC
calc.surv_ssC <- function(widedata, ...){
  widedata %>%
    .code_survbinsC() %>%
    
    # group_by(cohort, trt, trt.duration, trt.recover) %>%
    group_by(!!!rlang::ensyms(...)) %>%
    
    summarise( 
      ## surv per timept: alive at prev ##
      N_surv.enter24 = sum(status.enter == 1),
      N_surv.hs0 = sum(status.enter24 == 1), 
      N_surv.hs24 = sum(status.hs0 == 1), 
      N_surv.hs48 = sum(status.hs24 == 1), 
      N_surv.hs72 = sum(status.hs48 == 1), 
      
      ## death per timept: alive at prev + dead at current ##
      n_died.enter24 = sum(status.enter24 == 0),
      n_died.hs0 = sum(status.enter24 == 1 & status.hs0 == 0),
      n_died.hs24 = sum(status.hs0 == 1 & status.hs24 == 0),
      n_died.hs48 = sum(status.hs24 == 1 & status.hs48 == 0),
      n_died.hs72 = sum(status.hs48 == 1 & status.hs72 == 0),
      
      ## totals & proportions ##
      across(starts_with("status"), ~ sum(. == 0), .names = "{sub('status', 'N_died', col)}"),
      across(starts_with("status"), ~ sum(. == 1), .names = "{sub('status', 'n_surv', col)}"),
      across(starts_with("status"), ~ sum(status.enter == 1), .names = "{sub('status', 'N0', col)}"),
    ) %>%
    
    # A = initial/entered, a = final/remaining
    pivot_longer(cols = starts_with(c("N0", "n_")), names_to = c(".value", "timept"), names_sep = "\\.") %>%
    unique() %>%
    
    # A = cumulative, a = per timept
    mutate(P_died = N_died/N0,
           p_died = n_died/N_surv,
           P_surv = 1 - P_died,
           p_surv = 1 - p_died
    ) %>%
    pivot_longer(cols = starts_with(c("N_", "n_", "P_", "p_")),
                 names_to = c(".value", "status"), names_sep = "_")
  
}

## ver B2 -------------------------

# timept names (in hrs) based off hs
.code_survbins_hoursB2 <- function(widedata){
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
                              TRUE ~ 0)) 
}

# adds grouping by cohort (to grouping from B2a)
calc.surv_ssB2 <- function(widedata, ...){
  widedata %>%
    #dfs$r1$allB %>% # tester data
    .code_survbins_hoursB2() %>% #View()
    #group_by(cohort, trt, trt.duration, trt.recover) %>%
    group_by(!!!rlang::ensyms(...)) %>%
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

# calc.surv_ssB2a <- function(widedata){
#   widedata %>%
#     #dfs$r1$allB %>% # tester data
#     .code_survbins_hoursB2() %>% #View()
#     #group_by(trt, trt.duration, trt.recover) %>%
#     .count_n_pivotB2()
# }

## ver B -------------------------

# timept names (in hrs) based off enter/returning from recovery

.code_survbinsB <- function(widedata){
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
                                       TRUE ~ 0))
}

calc.surv_ssB <- function(widedata, ...){
  widedata %>%
    #dfs$r1$allB %>% # tester data
    .code_survbinsB() %>%
    #group_by(trt, trt.duration, trt.recover) %>%
    group_by(!!!rlang::ensyms(...)) %>%
    
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


## ver A (archived) -----------------------------------------------------------------

# TODO:refactor 2025-08-17: names are not up-to-date with the analysis script lol
# works with cohort A data only

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

