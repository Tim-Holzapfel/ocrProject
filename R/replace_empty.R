#' Replace Empty with Missing
#'
#' @description Function to replace empty strings with NA_character_.
#'
#' @param data_file Dataframe to be modified
#'
replace_empty <- function(data_file) {
  return_file <-
    data_file %>%
    dplyr::mutate_all(list(~ dplyr::na_if(., "")))

  return(return_file)
}
