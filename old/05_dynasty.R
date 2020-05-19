
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



str_det <- function(x) {

  which(str_detect(x, "dyn"))

}

stata_data <- haven::read_dta(file = "data/03_territories.dta", encoding = "UTF-8")

stata_sub <- stata_data %>% select(-c("ID", "Period", "N", "ID_group"))

# var_names <- str_subset(names(stata_data), "admin[0-9][0-9]?")
# stata_sub <- stata_data[, var_names]

test <- apply(stata_sub, 2, str_det)
index <- c(test[["admin2"]], test[["admin21"]], test[["admin22"]], test[["admin3"]]) %>% sort() %>%as_tibble()










str_dyn <- function(x) {

  str_which(x, "dyn")

}



test4 <- apply(stata_sub, 1, str_dyn)










test2 <- stata_sub[4930, ]

test2[str_which(test2, "dyn")]













