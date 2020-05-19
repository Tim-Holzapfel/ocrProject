
path_to_libraries <- "C:/Users/TimHo/Documents/R/R-Library" # REPLACE <username> with your user folder!
.libPaths(c(path_to_libraries, .libPaths()))

rm(list = ls())

setwd("D:/km/OCR_Project")

library(tidyverse)
library(magrittr)
library(tidyr)
library(stringi)
library(rlist)

stata_data <- haven::read_dta(file = "data/03_territories.dta", encoding = "UTF-8")

stata_data$General <- stata_data$Ruler %>% # whether the indicated person possessed some kind of military rank
  stri_detect_regex(
    "([gG]en\\.)|(Lt\\.)|(LtCol\\.)|(LtGen\\.)|(LtGov\\.)|(Brig\\.)|(Adm\\.)|(Capt\\.)|(Mar\\.)|(Comm\\.)|(Lic\\.)|(Commander)|(Milit\\.)|(Military)"
  ) %>%
  ifelse(1, 0)

stata_data$Executed <- stata_data$Ruler %>%
  stri_detect_regex(
    "exec\\."
  ) %>%
  ifelse(1, 0)


stata_data$Royal <- stata_data$Ruler %>% # c. = count, d. = duke, e. = earl
  stri_detect_regex(
    "(c\\.(?= [^0-9]))|( d\\.)|([bB]aron)|(bar\\.)|([pP]rince)|( e\\.)|(Emperor)"
  ) %>%
  ifelse(1, 0)




