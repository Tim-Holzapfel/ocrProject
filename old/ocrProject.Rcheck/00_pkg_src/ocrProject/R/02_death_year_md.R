#' @import stringi
#' @import stringr
#' @import dplyr

df_raw <- readRDS("data/01_excel_concat.RDS")


#' @title subset_ruler
#'
#' @param raw_file
#'
#' @return
#' @export
#'
#' @examples
subset_ruler <- function(raw_file = df_raw) {

  # subset of dataframe of IDs with three digits (ruler)

  df <- raw_file %>%
    dplyr::filter(stringr::str_detect(ID, "^[0-9][0-9][0-9]$")) %>%
    tidyr::drop_na(Ruler)

  df$ID %<>% as.integer()

  df$N <- 1:dim(df)[1]

  # index of ruler with known deathyear

  index_death <- stringr::str_which(df$Ruler, "[†‡]")

  df[, c("Death", "Murdered")] <- 0

  df[stringr::str_which(df$Ruler, "†"), "Death"] <- 1

  df[stringr::str_which(df$Ruler, "‡"), "Murdered"] <- 1

  # regular expression for a three or for digit number
  # surrounded by opening and closing brackets

  pattern <- "(\\([^\\(\\)]*\\d{3,4}[^\\)\\(]*\\))"

  # For those cases with a count of two the first year inside a bracket
  # is mostly a year that should not have been included inside a bracket.
  # In cases with a count equal to 2 the first bracket is therefore ignored.

  df$year <- df$Ruler %>%
    stringi::stri_extract_last(regex = pattern)


  # count of years inside brackets per observation.
  # most of the cases include only one bracket with year.
  # the observation is dropped if the count is higher than 2 because
  # those cases are rather rare but very difficult to allocate.

  df$period_count <- stringr::str_count(df$Ruler, pattern)

  print("Count of periods per Ruler")
  print(table(df$period_count))

  return(df)
}



df <- subset_ruler()






















