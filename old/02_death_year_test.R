#' @importFrom magrittr %>%
#' @import stringi
#' @import magrittr
#' @import stringr
#' @import dplyr



library(devtools)
library(usethis)


use_package(package = "purrr", type = "depends")



library(devtools)



df_raw <- readRDS("data/01_excel_concat.RDS")

df_raw$N <- 1:dim(df_raw)[1]

pattern <- "(\\([^\\(\\)]*\\d{3,4}[^\\)\\(]*\\))"
# regular expression for a three or for digit number
# surrounded by opening and closing brackets

df <- df_raw %>%
  filter(str_detect(ID, "^[0-9][0-9][0-9]$"))  # subset of dataframe of IDs with three digits (ruler)
