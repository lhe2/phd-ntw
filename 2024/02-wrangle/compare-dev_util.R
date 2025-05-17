# helpers_ntw-compare.R
# 2025-01-23

# dumping help functions and things for wrangling
# see 2024 entsoc files for some ideas/longevity stuff


## some useful generics ----------------------------------------------------

## calc se
se <- function(x){
  sd(na.omit(x))/sqrt(length(na.omit(x)))
}

## filters

filter.2pops <- function(data){
  data <- filter(data, !(diet == "TB" & diet == "lab"))
  
  return(data)
}

## calcing stats and then stitching it together ----------------------------

## 23v24 ntw stage-specific summary stats

### do calcs
calc.devstats <- function(data){ # use wide data
  
  # larval stats, ttpup = days
  ss_la <- data %>%
    group_by(year, pop, trt.type, minT,
    ) %>%
    summarise(avg.tt = mean(na.omit(tt.pupa)),
              se.tt = se(tt.pupa),
              # avg.tt = mean(na.omit(jdate.pupa - jdate.hatch)),
              # se.tt = sd(na.omit(jdate.pupa - jdate.hatch))/sqrt(length(na.omit(jdate.pupa - jdate.hatch))),
              # n = n() - sum(na.omit(surv.outcome == 2)),
              # n.pmd = sum(na.omit(surv.outcome == 0)), 
              # n.surv = n - n.pmd, 
              # prop.survpup = round(1-(n.pmd/n), digits=2),
              n = n(),
              n.misc = sum(na.omit(surv.outcome == 2)), # things that died by accident
              n.dev = n - n.misc, # larvae counted for growth stats
              n.pmd = sum(na.omit(surv.outcome == 0)),
              n.pup = sum(na.omit(surv.outcome == 1)),
              prop.survpup = round(n.pup/n.dev, digits = 2)
    ) %>% 
    mutate(stage = "la",
           z.type = "days",
           is.sep = "N")
  
  # larval stats, ttpup = 1/days
  ss_la2 <- data %>%
    group_by(year, pop, trt.type, minT,
    ) %>%
    summarise(avg.tt = mean(na.omit(1/tt.pupa)),
              se.tt = se(na.omit(1/tt.pupa)),
              n = n(),
              n.misc = sum(na.omit(surv.outcome == 2)), # things that died by accident
              n.dev = n - n.misc, # larvae counted for growth stats
              n.pmd = sum(na.omit(surv.outcome == 0)),
              n.pup = sum(na.omit(surv.outcome == 1)),
              prop.survpup = round(n.pup/n.dev, digits = 2)
    ) %>% 
    mutate(stage = "la",
           z.type = "rate",
           is.sep = "N")
  
  # pupal stats by sex
  ss_pu.sex <- data %>%
    filter(!is.na(sex)) %>%
    group_by(year, pop, sex, trt.type, minT) %>%
    summarise(avg.tt = mean(na.omit(tt.eclose)),
              se.tt = se(tt.eclose),
              # avg.tt = mean(na.omit(jdate.eclose - jdate.pupa)),
              # se.tt = sd(na.omit(jdate.eclose - jdate.pupa))/sqrt(length(na.omit(jdate.eclose - jdate.pupa))),
              avg.mass = mean(na.omit(mass.pupa)),
              se.mass = sd(na.omit(mass.pupa)/sqrt(length(na.omit(mass.pupa)))),
              n = n()) %>%
    mutate(stage = "pu",
           is.sep = "Y")
  
  # pupal stats combined sexes
  ss_pu <- data %>%
    filter(!is.na(jdate.pupa)) %>%
    group_by(year, pop, trt.type, minT) %>%
    summarise(avg.tt = mean(na.omit(tt.eclose)),
              se.tt = se(tt.eclose),
              # avg.tt = mean(na.omit(jdate.eclose - jdate.pupa)),
              # se.tt = sd(na.omit(jdate.eclose - jdate.pupa))/sqrt(length(na.omit(jdate.eclose - jdate.pupa))),
              avg.mass = mean(na.omit(mass.pupa)),
              se.mass = sd(na.omit(mass.pupa)/sqrt(length(na.omit(mass.pupa)))),
              n = n()) %>%
    mutate(stage = "pu",
           sex = "all",
           is.sep = "N")
  
  # adult stats by sex
  ss_ad.sex <- data %>%
    filter(!is.na(jdate.eclose)) %>%
    group_by(year, pop, trt.type, minT, sex) %>%
    summarise(avg.tt = mean(na.omit(tt.surv)),
              se.tt = se(tt.surv),
              # avg.tt = mean(na.omit(jdate.surv - jdate.eclose)),
              # se.tt = sd(na.omit(jdate.surv - jdate.eclose))/sqrt(length(na.omit(jdate.surv - jdate.eclose))),
              avg.mass = mean(na.omit(mass.eclose)),
              se.mass = sd(na.omit(mass.eclose)/sqrt(length(na.omit(mass.eclose)))),
              n = sum(na.omit(!is.na(jdate.eclose)))
    ) %>%
    mutate(stage = "ad",
           z.type = "final", 
           is.sep = "Y")
  
  # adult pct change in mass
  ss_ad.delta <- data %>%
    filter(!is.na(jdate.eclose)) %>%
    group_by(year, pop, trt.type, minT, sex) %>%
    summarise(avg.tt = mean(na.omit(tt.surv)),
              se.tt = se(tt.surv),
              # avg.tt = mean(na.omit(jdate.surv - jdate.eclose)),
              # se.tt = sd(na.omit(jdate.surv - jdate.eclose))/sqrt(length(na.omit(jdate.surv - jdate.eclose))),
              avg.mass = mean(na.omit((mass.eclose - mass.pupa)/mass.pupa*100)),
              se.mass = sd(na.omit((mass.eclose - mass.pupa)/mass.pupa*100)/sqrt(length(na.omit((mass.eclose - mass.pupa)/mass.pupa*100)))),
              n = sum(na.omit(!is.na(jdate.eclose)))
    ) %>%
    mutate(stage = "ad",
           z.type = "pdelta", 
           is.sep = "Y")
  
  # adult stats combined sexes
  ss_ad <- data %>%
    filter(!is.na(jdate.eclose)) %>%
    group_by(year, pop, trt.type, minT) %>%
    summarise(avg.tt = mean(na.omit(tt.surv)),
              se.tt = se(tt.surv),
              # avg.tt = mean(na.omit(jdate.surv - jdate.eclose)),
              # se.tt = sd(na.omit(jdate.surv - jdate.eclose))/sqrt(length(na.omit(jdate.surv - jdate.eclose))),
              avg.mass = mean(na.omit(mass.eclose)),
              se.mass = sd(na.omit(mass.eclose)/sqrt(length(na.omit(mass.eclose)))),
              n = sum(na.omit(!is.na(jdate.eclose)))
    ) %>%
    mutate(stage = "ad",
           sex = "all",
           z.type = "final",
           is.sep = "N")
  
  return(list(ss_la, ss_la2, ss_pu.sex, ss_pu, ss_ad.sex, ss_ad.delta, ss_ad))
  
}

### pull together
calc.ssadj <- function(data){ # use wide data
  data <- data %>%
    mutate(stage = factor(stage, levels = c("la", "pu", "ad")),
           avg.mass = avg.mass/1000,
           se.mass = se.mass/1000)
}



## 23v24 more dev stats
  # 2025-02-13: maybe smush into another fn lol

calc.ssmoredev <- function(data){ # use wide data
  moredev <- data %>%
    mutate(dmass = (mass.pupa - mass.eclose)/1000,
           masspupl = log(mass.pupa),
           rate.pup = (mass.pupa/tt.pupa)/1000,
           rate.puplog = log(mass.pupa)/tt.pupa)
  
  ss_moredev <- moredev %>%
    group_by(year, pop, trt.type, minT) %>%
    summarise(n = n(),
              avg.dmass = mean(na.omit(dmass)),
              se.dmass = se(dmass),
              avg.masspupl = mean(na.omit(masspupl)),
              se.masspupl = se(masspupl),
              avg.tt = mean(na.omit(tt.pupa)),
              se.tt = se(tt.pupa),
              avg.ratepup = mean(na.omit(rate.pup)),
              se.ratepup = se(rate.pup),
              avg.ratepupl = mean(na.omit(rate.puplog)),
              se.ratepupl = se(rate.puplog)) %>%
    mutate(sex = "both") # "f+m"
  
  ss_moredev.sex<- moredev %>%
    filter(!is.na(sex)) %>%
    group_by(year, pop, sex, trt.type, minT) %>%
    summarise(n = n(),
              avg.dmass = mean(na.omit(dmass)),
              se.dmass = se(dmass),
              avg.masspupl = mean(na.omit(masspupl)),
              se.masspupl = se(masspupl),
              avg.tt = mean(na.omit(tt.pupa)),
              se.tt = se(tt.pupa),
              avg.ratepup = mean(na.omit(rate.pup)),
              se.ratepup = se(rate.pup),
              avg.ratepupl = mean(na.omit(rate.puplog)),
              se.ratepupl = se(rate.puplog))
  
  return(Reduce(full_join, (list(ss_moredev, ss_moredev.sex))))
  
}


## 2v2 vs ntw instar-specific stats (needs long data)

### for comparing 2x2 vs ntw growth for 2025-05 bsft

calc.instats <- function(data){ # use long data
  
  data <- data %>%
    filter(is.sup == 0) %>%
    mutate(logmass = log(mass),
           devrate = mass/tt,
           ldevrate = logmass/tt)
  
  ss <- data %>%
    filter(instar %in% c("3rd", "4th", "5th", "pupa")) %>%
    group_by(pop, trt, instar, meanT, flucT) %>%
    summarise(n = n(),
              avg.mass = mean(na.omit(mass)), # mass in mg
              se.mass = se(mass),
              avg.logmass = mean(na.omit(logmass)),
              se.logmass = se(logmass),
              avg.tt = mean(tt),
              se.tt = se(tt),
              # # ttrate = 1/tt,
              # # avg.ttrate = mean(na.omit(ttrate)),
              # # se.ttrate = se(ttrate),
              avg.devrate = mean(na.omit(devrate)),
              se.devrate = se(devrate),
              avg.ldevrate = mean(na.omit(ldevrate)),
              se.ldevrate = se(ldevrate)
              ) %>%
    mutate(year = "both")
  
  ss_year <- data %>%
    filter(instar %in% c("3rd", "4th", "5th", "pupa")) %>%
    group_by(year, pop, trt, instar, meanT, flucT) %>%
    summarise(n = n(),
              avg.mass = mean(na.omit(mass)), # mass in mg
              se.mass = se(mass),
              avg.logmass = mean(na.omit(logmass)),
              se.logmass = se(logmass),
              avg.tt = mean(tt),
              se.tt = se(tt),
              avg.devrate = mean(na.omit(devrate)),
              se.devrate = se(devrate),
              avg.ldevrate = mean(na.omit(ldevrate)),
              se.ldevrate = se(ldevrate)
    )

  return(Reduce(full_join, (list(ss, ss_year))))
  
}



## instar-specific stats based off max/minTs

calc.instats2 <- function(data){ # use long data
  
  data <- data %>%
    filter(is.sup == 0) %>%
    mutate(logmass = log(mass),
           devrate = mass/tt,
           ldevrate = logmass/tt)
  
  ss <- data %>%
    filter(instar %in% c("3rd", "4th", "5th", "pupa")) %>%
    group_by(pop, instar, minT, trt.type) %>%
    summarise(n = n(),
              avg.mass = mean(na.omit(mass)), # mass in mg
              se.mass = se(mass),
              avg.logmass = mean(na.omit(logmass)),
              se.logmass = se(logmass),
              avg.tt = mean(tt),
              se.tt = se(tt),
              # # ttrate = 1/tt,
              # # avg.ttrate = mean(na.omit(ttrate)),
              # # se.ttrate = se(ttrate),
              avg.devrate = mean(na.omit(devrate)),
              se.devrate = se(devrate),
              avg.ldevrate = mean(na.omit(ldevrate)),
              se.ldevrate = se(ldevrate)
    ) %>%
    mutate(year = "both")
  
  ss_year <- data %>%
    filter(instar %in% c("3rd", "4th", "5th", "pupa")) %>%
    group_by(year, pop, instar, minT, trt.type) %>%
    summarise(n = n(),
              avg.mass = mean(na.omit(mass)), # mass in mg
              se.mass = se(mass),
              avg.logmass = mean(na.omit(logmass)),
              se.logmass = se(logmass),
              avg.tt = mean(tt),
              se.tt = se(tt),
              avg.devrate = mean(na.omit(devrate)),
              se.devrate = se(devrate),
              avg.ldevrate = mean(na.omit(ldevrate)),
              se.ldevrate = se(ldevrate)
    )
  
  return(Reduce(full_join, (list(ss, ss_year))))

}
  
