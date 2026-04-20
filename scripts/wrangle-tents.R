# ntw wrangle tents fns
# 2026-03-16

# utility fns for grouping + summary stats calcing
# (so can make custom subsets on the fly)

## load
source(here("scripts/math_utils.R"))

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
    group_by(across(c("year", "cage", starts_with(c("mate", "trt"))))) %>%
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
      ## per cage for each trt group
      avg.p.hatch.perc = mean(p.hatch.perc, na.rm = TRUE), # does this mess up the n.cages lol
      se.p.hatch.perc = seprop(avg.p.hatch.perc, n.cages.total),
      ## per trt group
      p.hatch = n.total.hatch/n.total.coll,
      se.hatch = seprop(p.hatch, n.total.coll),
      # until n.viable is handled properly, n.coll = n.viable
      # p.hatch = sum(n.hatch)/sum(n.viable), 
      # se.hatch = seprop(p.hatch, sum(n.viable))
    ) %>% 
    #filter(mate.type == "within", mate.pop == "lab", mate.trt == 426) %>%
    mutate(across(.cols = everything(), 
                  ~ replace(.x, is.nan(.x), NA))) %>% #View()
    ungroup()
}


