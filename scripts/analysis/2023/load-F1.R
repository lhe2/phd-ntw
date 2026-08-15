#2026-06-23

# load F1 data

source(here::here("scripts/R/tidy-dev.R"))

dfs_F1 <- list(
  wide = df_wide %>%
    filter(src %in% c("F1", "su")) %>%
    select(-c(instar.enter, 
              #cage, 
              if.mated, if.fridge,
              trt.group, is.ctrl, if.def, 
              jdate.tent, jdate.long, tt.long
    )) %>%
    mutate(tent = case_when(
                            parent.tent == "301-I" | id %in% c(1186, 1188, 1195, 1192, 1262, 1290, 1264, 1248) ~ "301-I",
                            parent.tent == "301-F" | id %in% c(1193, 1184, 1287, 1180) ~ "301-F",
                            parent.tent == "107-C" | id %in% c(1353, 1344, 1349, 1345, 1389) ~ "107-C"),
           # parent.trt = case_when(tent == "301-I" ~ "m/f @ 40-19C",
           #                        tent == "301-F" ~ "f @ 40-19C",
           #                        tent == "107-C" ~ "f @ 40-33C"),
           parent.minT = case_when(tent == "107-C" ~ 33,
                                  TRUE ~ 19),
           parent.trtsex = case_when(tent == "301-I" ~ "both",
                                     TRUE ~ "female"),
           pop = case_when(src == "su" ~ "parent",
                           src == "F1" ~ "F1")
           ) %>%
    drop_na(tent)
)

dfs_F1 <- list_modify(
  dfs_F1,
  long = dfs_F1$wide %>% ToLong()
)


rm(df_wide)