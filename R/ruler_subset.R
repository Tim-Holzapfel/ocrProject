#' @title Data subset only containing ruler
#'
#' @importFrom rlang .data
#'
#' @description Subsetting the data based on the id and selecting only
#'   observation that are composed of only three digits. These indicate the
#'   presence of an actual ruler as opposed to rows describing attributes of the
#'   aforementioned ruler.
#'
#' @param data Data set containing at least an id column and the names of rulers
#'   to subset for
#'
#'
#' @return Same data set as before but with fewer observations.
#'
ruler_subset <- function(data) {
  data_subset <-
    data %>%
    dplyr::filter(
      stringr::str_detect(.data$id, "^\\d{3}$") == TRUE
    ) %>%
    dplyr::mutate(
      id = gsub(" ", "", .data$id), # id can't contain empty spaces except headers!
      id = as.integer(.data$id)
    ) %>%
    tidyr::drop_na(.data$ruler)

  return(data_subset)
}
