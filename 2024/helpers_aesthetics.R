# 2025-02-12
# helpers_aesthetics.R

# quick ref of aesthetics objects that can be used for all this stuff lol 
# so i dont have to keep looking for them all the time


# temperatures

# ntw temps
# aes_4tempC <- c("26-26°C", "40-19°C", "40-26°C", "40-33°C")
# aes_4temp <- c("26-26", "40-19", "40-26", "40-33")
# aes_4tempcol <- c("#4393ce", "#fdae61", "#d73027", "#a50026")

aes_4temp <- list(c("26-26°C", "40-19°C", "40-26°C", "40-33°C"),
                  c("26-26", "40-19", "40-26", "40-33"),
                  c("#4393ce", "#fdae61", "#d73027", "#a50026"))


# aes_3tempC <- c("40-19°C", "40-26°C", "40-33°C")
# aes_3tempC <- c("40-19", "40-26", "40-33")
# aes_3tempcol <- c("#fdae61", "#d73027", "#a50026")

aes_3temp <- list(c("40-19°C", "40-26°C", "40-33°C"),
                  c("40-19", "40-26", "40-33"),
                  c("#fdae61", "#d73027", "#a50026"))



# life stages
# aes_stg <- c("larva", "pupa", "adult")
# aes_stgcol <- c("#1B9E77", "#D95F02", "#7570B3")

aes_stg <- list(c("larva", "pupa", "adult"),
                c("#1B9E77", "#D95F02", "#7570B3"))



# misc
# aes_2col <- c("#D95F02", "#7570B3")
# aes_3col <- c("#1B9E77", "#D95F02", "#7570B3")

aes_pal <- list(c("#4393ce", "#fdae61"),
                c("#1B9E77", "#D95F02", "#7570B3"))
