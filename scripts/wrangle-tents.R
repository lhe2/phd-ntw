# ntw wrangle tents fns
# 2026-03-16

# utility fns for grouping + summary stats calcing
# (so can make custom subsets on the fly)

## load
source(here("scripts/wrangle_utils.R"))

#' CalcTentCounts:
#' for modeling dfs (#s at the cage level) + SS for viz (to group by trt later)
#' misc TODO:
#' - should calc tent duration here, but would need to do the following:
#'  - need to do this with a df containing f.added and
#'    f.removed. (i.e. 1st day = 1st day of f.addition and 
#'    last day = day (after?) the final f was removed)
#'  - just doing `n()` ≠ days a tent existed, bc only counts the # of rows 
#'    that egg collection/hatch and m/f data are available for.
#'  - doing `last(jdate)-first(jdate)` isnt right either, bc
#'    some of the `jdate` rows are for hatch days that occur after the 
#'    tent was closed (i.e. last f was removed)

CalcTentCounts <- function(raw_df){
  raw_df %>%
    group_by(across(c("year", "cage", starts_with(c("mate", "trt")))), .add = TRUE) %>%
    summarise(
      n.f = sum(f.added),
      n.m = sum(m.added),
      #n.viable = sum(eggs.fert), # (dont need until counting fert eggs properly addressed)
      n.coll = sum(eggs.coll),
      n.coll.perf = n.coll/n.f,
      sqrt.coll.perf = sqrt(n.coll.perf),
      
      n.hatch = sum(eggs.hatched),
      n.hatch.perf = n.hatch/n.f,
      sqrt.hatch.perf = sqrt(n.hatch.perf),
      p.hatch.perc = n.hatch/n.coll
    ) %>%
    #filter(mate.type == "within", mate.pop == "lab", mate.trt == 426) %>% View()
    ungroup()
}

#' CalcTentSS:
#' for mate type combo SS (for viz). 
#' 
#' USAGE:
#' grpd_df: need to run CalcTentCounts beforehand and regroup at the `mate` and `trt` level.
#' 
CalcTentSS <- function(grpd_df){
  
  # # TODO TROUBLESHOOTING:
  # # trying to get it so can supply a dots argument (e.g."yr") and count_df
  # # but have a default grouping (across(mate, trt))
  # if(!missing(...)) {
  #   grp <- as.character(!!!rlang::enquos(...))
  #   
  #   grpd_df <- count_df %>%
  #     group_by(across(c(grp, starts_with(c("mate", "trt")))), .add = TRUE)
  # } else {
  #   grpd_df <- count_df %>%
  #     group_by(across(starts_with(c("mate", "trt"))))
  # }
  
  grpd_df %>%
    # # troubleshooting: should get 27 rows (w/ yr)
    # dfs_tidy$tents %>%
    # CalcTentCounts() %>% #View() # metrics at individ cage level
    # group_by(across(c("year", starts_with(c("mate", "trt"))))) %>%
    
    #group_by(across(starts_with(c("mate", "trt")))) %>%
    summarise(
      # grand totals
      n.cages.total = n(),
      n.f.total = sum(n.f),
      n.total.coll = sum(n.coll),
      n.total.hatch = sum(n.hatch),
      
      # eggs collected
      ## these look rly bad compared to the daily level LOL
      avg.coll.total = mean(n.coll, na.rm = TRUE),
      se.coll.total = se(n.coll),
      avg.coll.perf = mean(n.coll.perf, na.rm = TRUE),
      se.coll.perf = se(n.coll.perf),
      ## transformed
      avg.sqrt.coll.perf = mean(sqrt.coll.perf, na.rm = TRUE),
      se.sqrt.coll.perf = se(sqrt.coll.perf),
      avg.sqrt.hatch.perf = mean(sqrt.hatch.perf, na.rm = TRUE),
      se.sqrt.hatch.perf = se(sqrt.hatch.perf),
      
      # eggs hatched (overall count)
      avg.hatch = mean(n.hatch),
      se.hatch = se(n.hatch),
      # (doesnt rly make sense to have count per f?)
      
      # eggs hatched (props)
      #p.hatch.perc = n.hatch/n.coll,
      ## do earlier -- will get dups from prop.hatch per tent if not sum()'d beforehand
      ## per cage for each trt group (sample mean)
      avg.p.hatch.perc = mean(p.hatch.perc, na.rm = TRUE), # does this mess up the n.cages lol
      se.p.hatch.perc = seprop(avg.p.hatch.perc, n.cages.total),
      ## per trt group (population mean)
      avg.p.hatch.pop = n.total.hatch/n.total.coll,
      se.p.hatch.pop = seprop(avg.p.hatch.pop, n.total.coll),
      
      # until n.viable is handled properly, n.coll = n.viable
      # p.hatch = sum(n.hatch)/sum(n.viable), 
      # se.hatch = seprop(p.hatch, sum(n.viable))
    ) %>% 
    #filter(mate.type == "within", mate.pop == "lab", mate.trt == 426) %>%
    mutate(across(.cols = everything(), 
                  ~ replace(.x, is.nan(.x), NA))) %>% #View()
    ungroup()
}

#  ## TODO FUTZING CalcTentsSS: sth is wrong with the p_hatch lol
#.CalcTentsSS_testing <- function(x){
# dfs_tidy$tents %>% # for testing -- should get 37 rows
#   .CalcTentCounts() %>% #View() # metrics at individ cage level
#   group_by(across(c("year", starts_with(c("mate", "trt"))))) %>%
#   summarise(
#     # grand totals
#     ## see notes on calc'ing tent duration
#     #n.cages.total = n(), # TODO should be n.total.cages to match?
#     n.total_cages = n(),
#     #n.f.total = sum(n.f), # TODO should be n.total.f to match?
#     n.total_f = sum(n.f),
#     n.total_coll = sum(n.coll),
#     n.total_hatch = sum(n.hatch),
#     
#     # eggs collected
#     ## these look rly bad compared to the daily level LOL
#     avg_coll.total = mean(n.coll),
#     se_coll.total = se(n.coll),
#     avg_coll.perf = mean(n.coll.perf),
#     se_coll.perf = se(n.coll.perf),
#     ## transformed
#     avg_sqrt.coll.perf = mean(sqrt.coll.perf),
#     se_sqrt.coll.perf = se(sqrt.coll.perf),
#     avg_sqrt.hatch.perf = mean(sqrt.hatch.perf),
#     se_sqrt.hatch.perf = se(sqrt.hatch.perf),
#     
#     # eggs hatched (overall count)
#     avg_hatch = mean(n.hatch),
#     se_hatch = se(n.hatch),
#     # (doesnt rly make sense to have count per f?)
#     
#     # eggs hatched (props)
#     #p.hatch.perc = n.hatch/n.coll,
#     ## do earlier -- will get dups from prop.hatch per tent if not sum()'d beforehand
#     ## per cage for each trt group
#     avg_p.hatch.perc = mean(p.hatch.perc, na.rm = TRUE), # does this mess up the n.cages lol
#     se_p.hatch.perc = seprop(avg_p.hatch.perc, n.total_cages#n.cages.total
#     ),
#     ## per trt group
#     p.hatch = n.total_hatch/n.total_coll,
#     se_p.hatch = seprop(p.hatch, n.total_coll),
#     # until n.viable is handled properly, n.coll = n.viable
#     # p.hatch = sum(n.hatch)/sum(n.viable), 
#     # se.hatch = seprop(p.hatch, sum(n.viable))
#   ) %>%
#   #filter(mate.type == "within", mate.pop == "lab", mate.trt == 426) %>%
#   mutate(across(.cols = everything(), 
#                 ~ replace(.x, is.nan(.x), NA))) %>% #View()
#   pivot_longer(#starts_with(c("n", "avg", "se")),
#     contains("_"),
#     names_to = c(".value", "response"),
#     names_sep = "_") %>% View()
#}

# df prep -----------------------------------------------------------------

.GroupDefault <- function(x){
  x <- x %>%
    group_by(across(c(year, star??elits_with(c("mate", "trt", "is")))))
  
  if(rlang::has_name(x, "cage")){
    x <- x %>%
      group_by(across(cage), .add = TRUE)
  }
}

#' BLEHH this doesnt rly work.
#' by default, the cage-level SS needs to have "cage" anyway lol
#' for mate type combo-level, SS needs to omit the "trt.ishs" 
#'  (only cares about about `mate.*` and `trt.*` cols.)
#' and i think by default i'll always have the year in it anyway lol.
#' .. should see what viz-tents.Rmd actually needs lolll...
